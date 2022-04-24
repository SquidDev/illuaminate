type cache
type context

(** Construct a context from a map of modules and a "current" module. *)
val context :
  Doc_syntax.page Doc_syntax.documented Lazy.t Map.Make(String).t Map.Make(Namespace).t ->
  Doc_syntax.page Doc_syntax.documented option ->
  cache * context

val go_value_doc :
  cache:cache ->
  context ->
  Doc_syntax.value Doc_syntax.documented ->
  Doc_syntax.value Doc_syntax.documented

val go_page :
  cache:cache ->
  context ->
  Doc_syntax.page Doc_syntax.documented ->
  Doc_syntax.page Doc_syntax.documented
