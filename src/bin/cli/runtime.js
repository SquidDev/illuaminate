//Provides: unix_mkdir
function unix_mkdir(name, flags) {
  require("fs").mkdirSync(name.toString(), { mode: flags });
}
