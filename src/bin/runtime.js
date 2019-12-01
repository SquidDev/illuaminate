//Provides: unix_isatty
function unix_isatty(fd) {
  // TODO: This is available but unreleased.
  return typeof require == "function" && require("tty").isatty(fd);
}
