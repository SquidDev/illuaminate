//Provides: unix_isatty
function unix_isatty(fd) {
  // TODO: This is available but unreleased.
  return typeof require == "function" && require("tty").isatty(fd);
}

//Provides: caml_sys_get_config const
//Requires: caml_new_string
function caml_sys_get_config() {
  return [0, caml_new_string(joo_global_object.process.platform == "win32" ? "Win32" : "Unix"), 32, 0];
}

//Provides: unix_mkdir
function unix_mkdir(name, flags) {
  require("fs").mkdirSync(name.toString(), { mode: flags });
}
