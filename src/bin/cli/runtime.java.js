/****************************************************************************************************
 * argv support for Nashorn.
 ***************************************************************************************************/

//Provides: caml_argv
//Requires: caml_js_to_string
//Requires: raw_array_sub
var caml_argv = (function() {
  var g = joo_global_object;
  var main = "a.out";
  var args = [];

  if (g.process && g.process.argv && g.process.argv.length > 1) {
    var argv = g.process.argv;
    //nodejs
    main = argv[1];
    args = raw_array_sub(argv, 2, argv.length - 2);
  } else if (typeof g.arguments == "object") {
    args = g.arguments;
  }

  var p = caml_js_to_string(main);
  var args2 = [0, p];
  for (var i = 0; i < args.length; i++) args2.push(caml_js_to_string(args[i]));
  return args2;
})();

/****************************************************************************************************
 * Extend several functions to use caml_js_get_console instead of the console directly. Also make
 * caml_js_get_console support Rhino/Nashorn.
 ***************************************************************************************************/

// Provides: caml_js_get_console
function caml_js_get_console() {
  var g = joo_global_object;
  var c = g.console ? g.console : (g.console = {});
  if (!c.log) {
    if (g.Packages) {
      var out = g.Packages.java.lang.System.out;
      c.log = function(x) {
        out.println(x);
      };
    } else {
      c.log = function f() {};
    }
  }

  if (!c.error) {
    if (g.Packages) {
      var err = g.Packages.java.lang.System.err;
      c.error = function(x) {
        err.println(x);
      };
    } else {
      c.error = function f() {};
    }
  }
  return c;
}

//Provides: js_print_stdout (const)
//Requires: caml_utf16_of_utf8, caml_js_get_console
function js_print_stdout(s) {
  var s = caml_utf16_of_utf8(s);
  var g = joo_global_object;
  if (g.process && g.process.stdout && g.process.stdout.write) {
    g.process.stdout.write(s);
  } else {
    // Do not output the last \n if present
    // as console logging display a newline at the end
    if (s.charCodeAt(s.length - 1) == 10) s = s.substr(0, s.length - 1);
    caml_js_get_console().log(s);
  }
}

//Provides: js_print_stderr (const)
//Requires: caml_utf16_of_utf8, caml_js_get_console
function js_print_stderr(s) {
  var s = caml_utf16_of_utf8(s);
  var g = joo_global_object;
  if (g.process && g.process.stdout && g.process.stdout.write) {
    g.process.stderr.write(s);
  } else {
    // Do not output the last \n if present
    // as console logging display a newline at the end
    if (s.charCodeAt(s.length - 1) == 10) s = s.substr(0, s.length - 1);
    caml_js_get_console().error(s);
  }
}

//Provides: caml_sys_system_command
//Requires: caml_js_get_console
function caml_sys_system_command(cmd) {
  // Override this to use caml_js_get_console, and so work on Rhino.
  var cmd = cmd.toString();
  if (typeof require != "undefined" && require("child_process")) {
    try {
      require("child_process").execSync(cmd);
      return 0;
    } catch (e) {
      return 1;
    }
  }

  caml_js_get_console().log(cmd);
  return 127;
}

/****************************************************************************************************
 * Add support for Nashorn to the filesystem.
 ***************************************************************************************************/

//Provides: MlJavaDevice
//Requires: MlJavaFile
function MlJavaDevice(root) {
  this.Packages = joo_global_object.Packages;
  this.java = this.Packages.java;
  this.File = this.java.io.File;
  this.root = root;
}
MlJavaDevice.prototype.mk = function(name) {
  return new this.File(this.root + name);
};
MlJavaDevice.prototype.exists = function(name) {
  return this.mk(name).exists() ? 1 : 0;
};
MlJavaDevice.prototype.readdir = function(name) {
  // List, and then convert to JS strings.
  var files = [];
  var list = this.mk(name).list();
  for (var i = 0; i < list.length; i++) files[i] = "" + list[i];
  return files;
};
MlJavaDevice.prototype.is_dir = function(name) {
  return this.mk(name).isDirectory() ? 1 : 0;
};
MlJavaDevice.prototype.unlink = function(name) {
  var file = this.mk(name);
  return file.exists() && file.remove() ? 1 : 0;
};
MlJavaDevice.prototype.open = function(name, f) {
  var flags = new this.java.util.HashSet(2);
  var options = this.java.nio.file.StandardOpenOption;
  for (var key in f) {
    switch (key) {
      case "rdonly":
        flags.add(options.READ);
        break;
      case "wronly":
        flags.add(options.WRITE);
        break;
      case "append":
        flags.add(options.WRITE);
        flags.add(options.APPEND);
        break;
      case "create":
        flags.add(options.CREATE);
        break;
      case "truncate":
        flags.add(options.TRUNCATE_EXISTING);
        break;
      case "excl":
        flags.add(options.CREATE_NEW);
        break;
      // Skip binary, text, nonblock
    }
  }

  return new MlJavaFile(this.java.nio.channels.FileChannel.open(this.mk(name).toPath(), flags));
};

MlJavaDevice.prototype.rename = function(o, n) {
  this.mk(o).renameTo(this.mk(n));
};

MlJavaDevice.prototype.constructor = MlJavaDevice;

//Provides: MlJavaFile
//Requires: MlFile, caml_array_of_string, caml_bytes_set

function MlJavaFile(fd) {
  this.Packages = joo_global_object.Packages;
  this.ByteBuffer = this.Packages.java.nio.ByteBuffer;
  this.fd = fd;
}
MlJavaFile.prototype = new MlFile();

MlJavaFile.prototype.truncate = function(len) {
  this.fs.truncate(len);
};
MlJavaFile.prototype.length = function() {
  return this.fd.size();
};
MlJavaFile.prototype.write = function(offset, buf, buf_offset, len) {
  var a = caml_array_of_string(buf);
  if (!(a instanceof joo_global_object.Uint8Array)) a = new joo_global_object.Uint8Array(a);
  var buffer = this.ByteBuffer.allocate(len);
  for (var i = buf_offset; i < len; i++) buffer.put(a[i]);
  this.fd.write(buffer, offset);
  return 0;
};
MlJavaFile.prototype.read = function(offset, buf, buf_offset, len) {
  var a = caml_array_of_string(buf);
  if (!(a instanceof joo_global_object.Uint8Array)) a = new joo_global_object.Uint8Array(a);

  var buffer = this.ByteBuffer.allocate(len);
  var bytes = this.fd.read(buffer, offset);
  buffer.flip();
  for (var i = 0; i < bytes; i++) {
    caml_bytes_set(buf, i + buf_offset, buffer.get());
  }
  return 0;
};
MlJavaFile.prototype.read_one = function(offset) {
  var buffer = this.ByteBuffer.allocate(1);
  this.fd.read(buffer, offset);
  buffer.flip();
  return buffer.get();
};
MlJavaFile.prototype.close = function() {
  this.fd.close();
};

MlJavaFile.prototype.constructor = MlJavaFile;

//Provides: caml_current_dir
if (joo_global_object.process && joo_global_object.process.cwd) {
  var caml_current_dir = joo_global_object.process.cwd().replace(/\\/g, "/");
} else if (joo_global_object.Packages) {
  var caml_current_dir = new joo_global_object.Packages.java.io.File(".").getAbsolutePath();
} else {
  var caml_current_dir = "/static";
}
if (caml_current_dir.slice(-1) !== "/") caml_current_dir += "/";

//Provides:jsoo_mount_point
//Requires: MlFakeDevice, MlNodeDevice, MlJavaDevice, caml_root, fs_node_supported
var jsoo_mount_point = [];
if (fs_node_supported()) {
  jsoo_mount_point.push({
    path: caml_root,
    device: new MlNodeDevice(caml_root)
  });
} else if (typeof joo_global_object.Packages === "object") {
  jsoo_mount_point.push({
    path: caml_root,
    device: new MlJavaDevice(caml_root)
  });
} else {
  jsoo_mount_point.push({
    path: caml_root,
    device: new MlFakeDevice(caml_root)
  });
}
