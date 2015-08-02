#![feature(fs_walk)]
#![feature(path_ext)]

extern crate tempdir;

use std::cell::Cell;
use std::collections::VecDeque;
use std::default::Default;
use std::process::{Command};
use std::env::set_current_dir;
use std::fmt;
use std::path::{Path, PathBuf};

#[cfg(unix)]
use std::os::unix::fs::MetadataExt;

use tempdir::TempDir;

#[derive(Debug, Eq, PartialEq, Clone, Hash, Copy)]
pub enum NaClMode {
    Portable,
    Native(&'static str),
}
impl fmt::Display for NaClMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            &NaClMode::Portable => "le32",
            &NaClMode::Native(s) => s,
        };
        write!(f, "{}", s)
    }
}

#[cfg(all(unix, not(target_os = "macos")))]
fn get_platform_str() -> &'static str {
    "linux"
}
#[cfg(all(unix, target_os = "macos"))]
fn get_platform_str() -> &'static str {
    "mac"
}
#[cfg(windows)]
fn get_platform_str() -> &'static str {
    "win"
}

pub fn get_sdk_root() -> PathBuf {
    match getenv("NACL_SDK_ROOT") {
        None => panic!("Please set NACL_SDK_ROOT to your local pepper sdk"),
        Some(p) => Path::new(&p).to_path_buf(),
    }
}
pub fn get_nacl_target(v: &str) -> Option<NaClMode> {
    match v {
        "le32-unknown-nacl" => Some(NaClMode::Portable),
        "i686-unknown-nacl" => Some(NaClMode::Native("i686")),
        "x86_64-unknown-nacl" => Some(NaClMode::Native("x86_64")),
        "arm-unknown-nacl" => Some(NaClMode::Native("arm")),
        _ => None,
    }
}

fn getenv(v: &str) -> Option<String> {
    use std::env::var;
    use std::env::VarError::NotPresent;
    let r = match var(v) {
        Ok(v) => Some(v),
        Err(NotPresent) => None,
        Err(_) => {
            panic!("`{}` value isn't unicode", v);
        }
    };
    println!("{} = {:?}", v, r);
    r
}

mod non_nacl {
    use std::path::{Path, PathBuf};
    use super::getenv;
    fn get_var(var_base: &str) -> Result<String, String> {
        let target = getenv("TARGET")
            .expect("Environment variable 'TARGET' is unset");
        let host = match getenv("HOST") {
            None => { return Err("Environment variable 'HOST' is unset".to_string()); }
            Some(x) => x
        };
        let kind = if host == target { "HOST" } else { "TARGET" };
        let target_u = target.split('-')
            .collect::<Vec<&str>>()
            .connect("_");
        let res = getenv(&format!("{}_{}", var_base, target)[..])
            .or_else(|| getenv(&format!("{}_{}", var_base, target_u)[..]))
            .or_else(|| getenv(&format!("{}_{}", kind, var_base)[..]))
            .or_else(|| getenv(var_base));

        match res {
            Some(res) => Ok(res),
            None => Err("Could not get environment variable".to_string()),
        }
    }
    pub fn gcc(target: &str) -> PathBuf {
        let is_android = target.find("android").is_some();

        let r = get_var("CC").unwrap_or(if cfg!(windows) {
            "gcc".to_string()
        } else if is_android {
            format!("{}-gcc", target)
        } else {
            "cc".to_string()
        });
        Path::new(&r)
            .to_path_buf()
    }
    pub fn gxx(target: &str) -> PathBuf {
        let is_android = target.find("android").is_some();

        let r = get_var("CXX").unwrap_or(if cfg!(windows) {
            "g++".to_string()
        } else if is_android {
            format!("{}-g++", target)
        } else {
            "c++".to_string()
        });
        Path::new(&r)
            .to_path_buf()
    }

    pub fn ar(target: &str) -> PathBuf {
        let is_android = target.find("android").is_some();

        let r = get_var("AR").unwrap_or(if is_android {
            format!("{}-ar", target)
        } else {
            "ar".to_string()
        });
        Path::new(&r)
            .to_path_buf()
    }
    pub fn ranlib(target: &str) -> PathBuf {
        let is_android = target.find("android").is_some();

        let r = get_var("RANLIB").unwrap_or(if cfg!(windows) {
            "ranlib".to_string()
        } else if is_android {
            format!("{}-ranlib", target)
        } else {
            "ranlib".to_string()
        });
        Path::new(&r)
            .to_path_buf()
    }
}

#[derive(Clone, Hash)]
pub struct NativeTools {
    pub is_nacl: bool,

    pub cc:     PathBuf,
    pub cxx:    PathBuf,
    pub ar:     PathBuf,
    pub ranlib: PathBuf,
}
impl NativeTools {
    pub fn new(target: &str) -> NativeTools {
        let mode = get_nacl_target(target);
        if mode.is_none() {
            NativeTools {
                is_nacl: false,

                cc: non_nacl::gcc(&target[..]),
                cxx: non_nacl::gxx(&target[..]),
                ar: non_nacl::ar(&target[..]),
                ranlib: non_nacl::ranlib(&target[..]),
            }
        } else {
            let mode = mode.unwrap();
            let pepper = get_sdk_root().join("toolchain");

            let (cc, cxx, ar, ranlib) = match mode {
                NaClMode::Portable => {
                    let cc     = "pnacl-clang";
                    let cxx    = "pnacl-clang++";
                    let ar     = "pnacl-ar";
                    let ranlib = "pnacl-ranlib";

                    let tc: String = [get_platform_str(), "_pnacl"].concat();
                    let pepper: PathBuf = pepper.join(tc);
                    let pepper = pepper.join("bin");
                    (pepper.join(cc),
                     pepper.join(cxx),
                     pepper.join(ar),
                     pepper.join(ranlib))
                }
                NaClMode::Native(arch) if arch == "i686" || arch == "x86_64" => {
                    let cc: String     = [arch, "-nacl-gcc"].concat();
                    let cxx: String    = [arch, "-nacl-g++"].concat();
                    let ar: String     = [arch, "-nacl-ar"].concat();
                    let ranlib: String = [arch, "-nacl-ranlib"].concat();

                    let pepper = pepper
                        .join(format!("{}_x86_glibc", get_platform_str()));
                    let pepper = pepper.join("bin");
                    (pepper.join(cc),
                     pepper.join(cxx),
                     pepper.join(ar),
                     pepper.join(ranlib))
                }
                NaClMode::Native("arm") => {
                    let cc     = "arm-nacl-gcc";
                    let cxx    = "arm-nacl-g++";
                    let ar     = "arm-nacl-ar";
                    let ranlib = "arm-nacl-ranlib";

                    let pepper = pepper
                        .join(format!("{}_arm_newlib", get_platform_str()));
                    let pepper = pepper.join("bin");

                    (pepper.join(cc),
                     pepper.join(cxx),
                     pepper.join(ar),
                     pepper.join(ranlib))
                }
                NaClMode::Native(_) => unreachable!(),
            };

            NativeTools {
                is_nacl: true,

                cc: cc,
                cxx: cxx,
                ar: ar,
                ranlib: ranlib,
            }
        }
    }
}

impl Default for NativeTools {
    fn default() -> NativeTools {
        let target = getenv("TARGET").unwrap();
        NativeTools::new(&target[..])
    }
}

#[cfg(not(target_os = "nacl"))]
pub fn print_lib_paths() {
    let mode = get_nacl_target(getenv("TARGET").unwrap().as_ref());
    if mode.is_none() { return; }
    let mode = mode.unwrap();

    let root = get_sdk_root();
    let pepper = root.join("toolchain");
    let tc: String = [get_platform_str(), "_pnacl"].concat();
    let mut pepper: PathBuf = pepper.join(tc);
    let (ports, arch, lib) = match mode {
        NaClMode::Portable => ("pnacl", "le32", "lib"),
        _ => unimplemented!(),
    };
    pepper.push(format!("{}-nacl", arch));
    let main = pepper.join(lib);
    let installed = pepper.join("usr/lib");
    let ports = root.join("lib")
        .join(ports)
        .join(if getenv("DEBUG") == Some("true".to_string()) {
            "Debug"
        } else {
            "Release"
        });

    println!("cargo:rustc-link-search=native={}", main.display());
    println!("cargo:rustc-link-search=native={}", installed.display());
    println!("cargo:rustc-link-search=native={}", ports.display());
}
#[cfg(target_os = "nacl")]
pub fn print_lib_paths() { }

#[cfg(not(target_os = "nacl"))]
pub fn set_pkg_config_envs() {
    use std::env;
    let mode = get_nacl_target(getenv("TARGET").unwrap().as_ref());
    if mode.is_none() { return; }
    let mode = mode.unwrap();

    let pepper = get_sdk_root().join("toolchain");
    let tc: String = [get_platform_str(), "_pnacl"].concat();
    let mut pepper: PathBuf = pepper.join(tc);
    let arch = match mode {
        NaClMode::Portable => "le32",
        _ => unimplemented!(),
    };
    pepper.push(format!("{}-nacl", arch));
    pepper.push("usr/lib/pkgconfig");
    //$NACL_SDK_ROOT/toolchain/$($NACL_SDK_ROOT/tools/getos.py)_pnacl/le32-nacl/usr/lib/pkgconfig

    let mut new_paths: Vec<PathBuf> = Vec::with_capacity(1);
    new_paths.push(pepper);
    if let Some(paths) = getenv("PKG_CONFIG_LIBDIR") {
        new_paths.extend(env::split_paths(&paths))
    }
    env::set_var("PKG_CONFIG_LIBDIR", env::join_paths(new_paths).unwrap());
    env::set_var("PKG_CONFIG_ALLOW_CROSS", "1");
}
#[cfg(target_os = "nacl")]
pub fn set_pkg_config_envs() { }

pub struct ConfigureMake {
    tools: NativeTools,
    args:  Vec<String>,
    built_libs: VecDeque<(PathBuf, String)>,
    make_only_dirs: Option<VecDeque<PathBuf>>,

    out_dir: PathBuf,
    src_dir: PathBuf,

    fresh: Cell<Option<bool>>,
}
impl ConfigureMake {
    pub fn new(args: &[String],
               built_libs: &[(PathBuf, String)],
               src_dir: PathBuf) -> ConfigureMake {
        let sdk = get_sdk_root();
        let triple = getenv("TARGET").unwrap();
        let target = get_nacl_target(&triple[..]);

        let tools: NativeTools = NativeTools::new(&triple[..]);

        let extra_flags = format!("-I{}/include",
                                  sdk.display());
        let extra_flags = match target {
            Some(NaClMode::Portable) => format!("{} -I{}/include/pnacl",
                                                extra_flags, sdk.display()),
            _ => extra_flags,
        };
        let extra_flags = if tools.is_nacl { format!("{} -D__USE_GNU", extra_flags) }
                          else             { extra_flags };
        let args = args.iter()
            .map(|str| {
                if str.starts_with("CFLAGS=") ||
                    str.starts_with("CXXFLAGS=") {
                    format!("{} {}", str, extra_flags)
                } else {
                    str.to_string()
                }
            }).collect();

        ConfigureMake {
            tools: tools,
            args:  args,
            built_libs: built_libs.iter().map(|&(ref p, ref l): &(PathBuf, String)| {
                assert!(p.is_relative());
                // map old style `foo:static` into `static=foo`.
                let mut split = l.split(':');
                let l = if let Some(lib) = split.next() {
                    let mut lib = lib.to_string();
                    let mut kind = if let Some(k) = split.next() {
                        k
                    } else {
                        return (p.clone(), l.clone());
                    };
                    loop {
                        let next = match split.next() {
                            None => { break; },
                            Some(next) => next,
                        };
                        lib = format!("{}:{}", lib, kind);
                        kind = next;
                    }
                    format!("{}={}", kind, lib)
                } else {
                    l.clone()
                };
                (p.clone(), l.clone())
            }).collect(),
            make_only_dirs: None,

            out_dir: Path::new(&getenv("OUT_DIR").unwrap()).to_path_buf(),
            src_dir: src_dir,

            fresh: Cell::new(None),
        }
    }
    pub fn make_only_dir(&mut self, dir: PathBuf) -> &mut ConfigureMake {
        let mut m = self.make_only_dirs
            .take()
            .unwrap_or_else(|| VecDeque::new() );

        m.push_front(dir);
        self.make_only_dirs = Some(m);
        self
    }
    pub fn make_all(&mut self) {
        self.make_only_dirs = None;
    }

    /// Do we need to rebuild this?
    pub fn is_fresh(&self) -> bool {
        use std::fs::{metadata, walk_dir, read_dir};
        use std::cmp::{max, min};
        if self.fresh.get().is_some() {
            return self.fresh.get().unwrap();
        } else {
            println!("checking freshness:");
            // get the mtime of the outputs:
            let built_libs: Vec<PathBuf> = self.built_libs
                .iter()
                .map(|&(ref p, ref s)| (p, s.split(':').next().expect("invalid extern lib spec?")) )
                // TODO: PNaCl always uses lib.a, but this convention is not constant for other targets.
                .map(|(p, s)| p.join(format!("lib{}.a", s)) )
                .collect();

            let mut oldest_lib: i64 = i64::max_value();
            for lib in built_libs.into_iter() {
                let lib = self.out_dir.join(lib);
                let stat = metadata(&lib);
                if stat.is_err() {
                    println!("not fresh: stat on output library failed: `{}`", lib.display());
                    // not fresh. non-existant files will hit this too.
                    self.fresh.set(Some(false));
                    return false;
                }

                let stat = stat.unwrap();
                oldest_lib = min(oldest_lib, stat.mtime() as i64);
            }

            let mut newest_timestamp: i64 = 0;
            let dir_iter = walk_dir(&self.src_dir).unwrap();
            for dir in dir_iter {
                if dir.is_err() { continue; }
                let dir = dir.unwrap().path();
                let files = read_dir(&dir);
                if !files.is_ok() { continue; }

                for file in files.unwrap() {
                    if file.is_err() { continue; }
                    let file = file.unwrap().path();
                    let stat = metadata(&file);
                    if !stat.is_ok() { continue; }
                    let stat = stat.unwrap();

                    newest_timestamp = max(newest_timestamp, stat.mtime() as i64);
                }
            }

            if oldest_lib >= newest_timestamp {
                // fresh
                println!("fresh: `{}` >= `{}`!", oldest_lib, newest_timestamp);
                self.fresh.set(Some(true));
            } else {
                println!("not fresh: `{}` < `{}`!", oldest_lib, newest_timestamp);
                // not fresh
                self.fresh.set(Some(false));
            }
            println!("");
            println!("");
            return self.fresh.get().unwrap();
        }
    }

    pub fn configure(&self) {
        if self.is_fresh() { return; }

        let cfg = self.src_dir.join("configure");

        set_current_dir(&self.out_dir).unwrap();

        let mut cmd = Command::new(&cfg);
        cmd.args(&self.args[..]);

        cmd.arg(format!("--host={}",
                        getenv("TARGET").unwrap()));

        let cc_arg = format!("CC={}",
                             self.tools.cc.display());
        let cxx_arg = format!("CXX={}",
                              self.tools.cxx.display());
        let ar_arg = format!("AR={}",
                             self.tools.ar.display());
        let ranlib_arg = format!("RANLIB={}",
                                 self.tools.ranlib.display());
        cmd.arg(cc_arg);
        cmd.arg(cxx_arg);
        cmd.arg(ar_arg);
        cmd.arg(ranlib_arg);

        run_tool(cmd);

        println!("");
        println!("");

        set_current_dir(&self.src_dir).unwrap();
    }

    pub fn make(self) {
        if !self.is_fresh() {
            let make = getenv("MAKE")
                .unwrap_or_else(|| "make".to_string() );
            let make_prog = Path::new(&make);

            set_current_dir(&self.out_dir).unwrap();

            let mut cmd = Command::new(&make_prog);

            cmd.arg("-j")
                .arg(getenv("NUM_JOBS").unwrap_or_else(|| "1".to_string() ));

            match self.make_only_dirs {
                Some(ref dirs) => {
                    for dir in dirs.iter() {
                        let mut cmd = Command::new(&make_prog);

                        cmd.arg("-j")
                            .arg(getenv("NUM_JOBS").unwrap_or_else(|| "1".to_string() ));

                        cmd.arg("-C")
                            .arg(dir.display().to_string());
                        run_tool(cmd);
                    }
                }
                None => run_tool(cmd),
            }

            println!("");
            println!("");

            set_current_dir(&self.src_dir).unwrap();
        }

        for (p, l) in self.built_libs.into_iter() {
            let p = self.out_dir.join(p);
            println!("cargo:rustc-link-lib={}", l);
            println!("cargo:rustc-link-search=native={}",
                     p.display());
            println!("cargo:libdir={}", p.display());
        }
    }
}

fn run_tool(mut cmd: Command) {
    use std::process::Stdio;

    println!("{:?}", cmd);

    cmd.stdout(Stdio::inherit());
    cmd.stderr(Stdio::inherit());
    assert!(cmd.status().unwrap().success());
}

pub struct Archive {
    tools: NativeTools,

    tmp: TempDir,
    doubles: u64,
    obj_files: VecDeque<PathBuf>,
    libname: String,
    output: PathBuf,
}
impl Archive {
    pub fn new(out_stem: &str) -> Archive {
        let out_dir = getenv("OUT_DIR").unwrap();

        let d: String = ["lib", out_stem, "-objs"].concat();
        let lib_file: String = ["lib", out_stem, ".a"].concat();
        Archive {
            tools: Default::default(),

            tmp: TempDir::new(&d[..]).unwrap(),
            doubles: 1,
            obj_files: VecDeque::new(),
            libname: out_stem.to_string(),
            output: Path::new(&out_dir).join(lib_file),
        }
    }

    fn src_obj(&mut self, src: &str) -> (PathBuf, PathBuf) {
        use std::fs::PathExt;

        let src = Path::new(src);
        let stem = src.file_stem()
            .expect("provided source is not a filename");
        let obj = Path::new(stem).with_extension("o");
        let obj = self.tmp.path().join(obj);
        let obj = if obj.exists() {
            let obj_str = format!("{}{}.o",
                                  stem.to_str().unwrap(),
                                  {
                                      let d = self.doubles;
                                      self.doubles += 1;
                                      d
                                  });
            let obj = Path::new(&obj_str);
            self.tmp.path().join(obj)
        } else {
            obj
        };
        self.obj_files.push_front(obj.clone());
        (src.to_path_buf(), obj)
    }

    fn run(&mut self, cmd: Command) -> &mut Archive {
        run_tool(cmd);
        self
    }
    fn nacl_build_cflags() -> Vec<String> {
        let mut a = Vec::new();

        let sdk = get_sdk_root();
        let triple = getenv("TARGET").unwrap();
        let target = get_nacl_target(&triple[..]);

        a.push(format!("-I{}/include", sdk.display()));
        match target {
            Some(NaClMode::Portable) => {
                a.push(format!("-I{}/include/pnacl", sdk.display()))
            }
            _ => (),
        }
        a
    }

    pub fn cc(&mut self, src: &str, args: &[String]) -> &mut Archive {
        let (src, obj) = self.src_obj(src);

        let mut cmd = Command::new(&self.tools.cc);
        cmd.arg("-c")
            .arg(src)
            .arg("-o")
            .arg(obj)
            .args(args);

        if self.tools.is_nacl {
            cmd.args(&Archive::nacl_build_cflags()[..]);
        }

        self.run(cmd)
    }

    pub fn cxx(&mut self, src: &str, args: &[String]) -> &mut Archive {
        let (src, obj) = self.src_obj(src);

        let mut cmd = Command::new(&self.tools.cxx);
        cmd.arg("-c")
            .arg(src)
            .arg("-o")
            .arg(obj)
            .args(args);

        if self.tools.is_nacl {
            cmd.args(&Archive::nacl_build_cflags()[..]);
        }

        self.run(cmd)
    }

    pub fn archive(mut self) {
        let ar_cmds = "crus";

        let mut cmd = Command::new(&self.tools.ar);
        cmd.arg(ar_cmds)
            .arg(self.output.clone());
        for f in self.obj_files.iter() {
            cmd.arg(f.display().to_string());
        }
        self.run(cmd);

        let out_dir = self.output.parent().unwrap();
        println!("cargo:rustc-link-search=native={}", out_dir.display());
        println!("cargo:rustc-link-lib=static={}", self.libname);
    }
}
