(rule (deps (glob_files extra/*.lua)) (action (with-stdout-to t_minify.new (with-stdin-from %{dep:t_minify.md} (run %{dep:./main.exe} md)))))
(rule (alias t_minify) (action (diff t_minify.md t_minify.new)))

(alias
 (name runtest)
 (deps
  (alias t_minify)))
