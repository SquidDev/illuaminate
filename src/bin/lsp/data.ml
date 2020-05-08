open IlluaminateData

let src = Logs.Src.create ~doc:"Loading and storing of local files" __MODULE__

module Log = (val Logs.src_log src)

let rate_limit (type k v) ~(delay : Mtime.span) :
    (k, v, context -> k -> v Key.reason -> v Key.result) Key.factory =
 fun ~name ?pp ?container f ->
  let compute =
    Key.builtin ~name:(name ^ ".compute") ?pp ?container @@ fun store key previous ->
    match previous with
    | Absent ->
        let result = f store key Absent in
        { value = (Mtime_clock.now (), result.value); changed = RecomputeChange }
    | DependencyChange (_, v) ->
        (* If a dependency changed, always recompute. *)
        let result = f store key (DependencyChange v) in
        { value = (Mtime_clock.now (), result.value); changed = RecomputeChange }
    | Recompute ((t, v) as value) ->
        if Mtime.Span.compare (Mtime.span (Mtime_clock.now ()) t) delay < 0 then
          { value; changed = NoChange }
        else
          let result = f store key (Recompute v) in
          { value = (Mtime_clock.now (), result.value); changed = RecomputeChange }
  in

  Key.key ~name ?pp ?container (fun store k -> need store compute k |> snd)

module FileDigest = struct
  type 'a t =
    { digest : Digest.t;
      time : Mtime.t;
      value : 'a
    }

  let default_duration = Mtime.Span.of_uint64_ns 30_000_000_000L (* 30s *)

  let equal ~eq l r = l == r || (l.digest = r.digest && l.time = r.time && eq l.value r.value)

  (** Read a file and perform some processing function (such as parsing) if the contents has changed
      since last time we checked. Limited to 5 seconds. *)
  let with_change ?(delay = default_duration) ~process ~path (previous : 'a t option) =
    let path_s = Fpath.to_string path in
    let error msg =
      Log.warn (fun f -> f "Error reading file %S (%s)" path_s msg);
      None
    in
    let result ~digest value = Some { digest; time = Mtime_clock.now (); value } in
    let read ~new_digest =
      match open_in path_s with
      | exception Sys_error e -> error e
      | ic -> (
        match process ic with
        | res -> close_in ic; result ~digest:new_digest res
        | exception Sys_error e -> close_in_noerr ic; error e
        | exception e ->
            let bt = Printexc.get_raw_backtrace () in
            close_in ic;
            Printexc.raise_with_backtrace e bt )
    in

    match previous with
    | Some ({ time; _ } as result)
      when Mtime.Span.compare (Mtime.span (Mtime_clock.now ()) time) delay < 0 ->
        (* Skip if we changed in strictly less than "delay" time. *)
        Some result
    | Some { digest; value; _ } -> (
      match Digest.file path_s with
      | new_digest ->
          Log.debug (fun f ->
              f "Checking digest for %S (previously %s, now %s)" path_s (Digest.to_hex digest)
                (Digest.to_hex new_digest));
          if new_digest = digest then result ~digest value else read ~new_digest
      | exception Sys_error msg -> error msg )
    | None -> (
      match Digest.file path_s with
      | new_digest -> read ~new_digest
      | exception Sys_error msg -> error msg )

  let oracle ?delay ?container ?(eq = ( == )) ~name process =
    let compute path previous =
      Option.join previous |> with_change ?delay ~process:(process path) ~path
    in
    let compute_key =
      Key.oracle ~pp:Fpath.pp
        ~eq:(Option.equal (equal ~eq))
        ?container ~name:(name ^ ".compute") compute
    in
    Key.key ~pp:Fpath.pp ?container ~eq:(Option.equal eq) ~name @@ fun store key ->
    need store compute_key key |> Option.map (fun x -> x.value)
end
