#![allow(unstable)]

extern crate libc;

use std::cell::Cell;
use std::collections::RingBuf;
use std::default::Default;
use std::io::{Command, TempDir};
use std::os::{getenv, change_dir};

#[derive(Show, Eq, PartialEq, Clone, Hash, Copy)]
pub enum Mode {
    Portable,
    Native(&'static str),
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
    "windows"
}

pub fn get_sdk_root() -> Path {
    match getenv("NACL_SDK_ROOT") {
        None => panic!("Please set NACL_SDK_ROOT to your local pepper sdk"),
        Some(p) => Path::new(p),
    }
}
pub fn get_nacl_target() -> Option<Mode> {
    getenv("TARGET").and_then(|v| {
        match v.as_slice() {
            "le32-unknown-nacl" => Some(Mode::Portable),
            "i686-unknown-nacl" => Some(Mode::Native("i686")),
            "x86_64-unknown-nacl" => Some(Mode::Native("x86_64")),
            "arm-unknown-nacl" => Some(Mode::Native("arm")),
            _ => None,
        }
    })
}

#[derive(Clone, Hash)]
pub struct NativeTools {
    cc:     Path,
    cxx:    Path,
    ar:     Path,
    ranlib: Path,
}

impl Default for NativeTools {
    fn default() -> NativeTools {
        let mode = get_nacl_target().expect("unknown target");

        let pepper = get_sdk_root().join("toolchain");

        let (cc, cxx, ar, ranlib) = match mode {
            Mode::Portable => {
                let cc     = "pnacl-clang";
                let cxx    = "pnacl-clang++";
                let ar     = "pnacl-ar";
                let ranlib = "pnacl-ranlib";

                let tc: String = [get_platform_str(), "_pnacl"].concat();
                let pepper: Path = pepper.join(tc);
                let pepper = pepper.join("bin");
                (pepper.join(cc),
                 pepper.join(cxx),
                 pepper.join(ar),
                 pepper.join(ranlib))
            }
            Mode::Native(arch) if arch == "i686" || arch == "x86_64" => {
                let cc: String     = [arch, "-nacl-gcc"].concat();
                let cxx: String    = [arch, "-nacl-g++"].concat();
                let ar: String     = [arch, "-nacl-ar"].concat();
                let ranlib: String = [arch, "-nacl-ranlib"].concat();

                let pepper = pepper
                    .join_many(&[[get_platform_str(), "_x86_glibc"].concat(),
                                 "bin".to_string()]);
                (pepper.join(cc),
                 pepper.join(cxx),
                 pepper.join(ar),
                 pepper.join(ranlib))
            }
            Mode::Native("arm") => {
                let cc     = "arm-nacl-gcc";
                let cxx    = "arm-nacl-g++";
                let ar     = "arm-nacl-ar";
                let ranlib = "arm-nacl-ranlib";

                let pepper = pepper
                    .join_many(&[[get_platform_str(), "_arm_newlib"].concat(),
                                 "bin".to_string()]);
                (pepper.join(cc),
                 pepper.join(cxx),
                 pepper.join(ar),
                 pepper.join(ranlib))
            }
            Mode::Native(_) => unreachable!(),
        };

        NativeTools {
            cc: cc,
            cxx: cxx,
            ar: ar,
            ranlib: ranlib,
        }
    }
}

pub struct ConfigureMake {
    tools: NativeTools,
    args:  Vec<String>,
    built_libs: Vec<(Path, String)>,
    make_only_dirs: Option<Vec<Path>>,

    out_dir: Path,
    src_dir: Path,

    fresh: Cell<Option<bool>>,
}
impl ConfigureMake {
    pub fn new(args: &[String],
               built_libs: &[(Path, String)],
               src_dir: Path) -> ConfigureMake {
        let sdk = get_sdk_root();
        let target = get_nacl_target();

        let extra_flags = format!("-I{}/include",
                                  sdk.display());
        let extra_flags = match target {
            Some(Mode::Portable) => format!("{} -I{}/include/pnacl",
                                            extra_flags, sdk.display()),
            _ => extra_flags,
        };
        let extra_flags = format!("{} -D__USE_GNU", extra_flags);
        let args = args.iter()
            .map(|str| {
                if str.as_slice().starts_with("CFLAGS=") ||
                    str.as_slice().starts_with("CXXFLAGS=") {
                    format!("{} {}", str, extra_flags)
                } else {
                    str.to_string()
                }
            }).collect();

        ConfigureMake {
            tools: Default::default(),
            args:  args,
            built_libs: built_libs.iter().map(|&(ref p, ref l): &(Path, String)| {
                assert!(p.is_relative());
                (p.clone(), l.clone())
            }).collect(),
            make_only_dirs: None,

            out_dir: Path::new(getenv("OUT_DIR").unwrap()),
            src_dir: src_dir,

            fresh: Cell::new(None),
        }
    }
    pub fn make_only_dir(&mut self, dir: Path) -> &mut ConfigureMake {
        let mut m = self.make_only_dirs
            .take()
            .unwrap_or_else(|| Vec::new() );

        m.push(dir);
        self.make_only_dirs = Some(m);
        self
    }
    pub fn make_all(&mut self) {
        self.make_only_dirs = None;
    }

    /// Do we need to rebuild this?
    pub fn is_fresh(&self) -> bool {
        use std::io::fs::{walk_dir, readdir, stat};
        use std::io::fs::PathExtensions;
        use std::cmp::{max, min};
        use std::num::Int;
        if self.fresh.get().is_some() {
            return self.fresh.get().unwrap();
        } else {
            println!("checking freshness:");
            // get the mtime of the outputs:
            let built_libs: Vec<Path> = self.built_libs
                .iter()
                .map(|&(ref p, ref s)| (p, s.split(':').next().expect("invalid extern lib spec?")) )
                // TODO: PNaCl always uses lib.a, but this convention is not constant for other targets.
                .map(|(p, s)| p.join(format!("lib{}.a", s)) )
                .collect();

            let mut oldest_lib: u64 = Int::max_value();
            for lib in built_libs.into_iter() {
                let lib = self.out_dir.join(lib);
                let stat = lib.stat();
                if stat.is_err() {
                    println!("not fresh: stat on output library failed: `{:?}`", lib.display());
                    // not fresh. non-existant files will hit this too.
                    self.fresh.set(Some(false));
                    return false;
                }

                let stat = stat.unwrap();
                oldest_lib = min(oldest_lib, stat.modified);
            }

            let mut newest_timestamp: u64 = 0;
            let mut dir_iter = walk_dir(&self.src_dir).unwrap();
            for dir in dir_iter {
                let files = readdir(&dir);
                if !files.is_ok() { continue; }

                for file in files.unwrap().into_iter() {
                    let stat = stat(&file);
                    if !stat.is_ok() { continue; }
                    let stat = stat.unwrap();

                    newest_timestamp = max(newest_timestamp, stat.modified);
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

        assert!(change_dir(&self.out_dir).is_ok());

        let mut cmd = Command::new(&cfg);
        cmd.args(self.args.as_slice());

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

        assert!(change_dir(&self.src_dir).is_ok());
    }

    pub fn make(self) {
        if !self.is_fresh() {
            let make_prog = Path::new(getenv("MAKE").unwrap_or_else(|| "make".to_string() ));

            assert!(change_dir(&self.out_dir).is_ok());

            let mut cmd = Command::new(&make_prog);

            cmd.arg("-j")
                .arg(getenv("NUM_JOBS").unwrap_or_else(|| "1".to_string() ));

            match self.make_only_dirs {
                Some(ref dirs) => {
                    for dir in dirs.iter() {
                        let mut dir_cmd = cmd.clone();
                        dir_cmd.arg("-C")
                            .arg(dir.display().to_string());
                        run_tool(dir_cmd);
                    }
                }
                None => run_tool(cmd),
            }

            println!("");
            println!("");

            assert!(change_dir(&self.src_dir).is_ok());
        }

        for (p, l) in self.built_libs.into_iter() {
            let p = self.out_dir.join(p);
            println!("cargo:rustc-flags=-L {} -l {}",
                     p.display(), l);
            println!("cargo:libdir={}", p.display());
        }
    }
}

fn run_tool(mut cmd: Command) {
    use libc;
    use std::io::process::InheritFd;

    println!("{}", cmd);

    cmd.stdout(InheritFd(libc::STDOUT_FILENO));
    cmd.stderr(InheritFd(libc::STDERR_FILENO));
    assert!(cmd.status().unwrap().success());
}

pub struct Archive {
    cc:  Path,
    cxx: Path,
    ar:  Path,
    ranlib: Path,

    tmp: TempDir,
    doubles: u64,
    obj_files: RingBuf<Path>,
    libname: String,
    output: Path,
}
impl Archive {
    pub fn new(out_stem: &str) -> Archive {
        let NativeTools {
            cc, cxx, ar, ranlib,
        } = Default::default();

        let out_dir = getenv("OUT_DIR").unwrap();

        let d: String = ["lib", out_stem, "-objs"].concat();
        let lib_file: String = ["lib", out_stem, ".a"].concat();
        Archive {
            cc:     cc,
            cxx:    cxx,
            ar:     ar,
            ranlib: ranlib,

            tmp: TempDir::new(d.as_slice()).unwrap(),
            doubles: 1,
            obj_files: RingBuf::new(),
            libname: out_stem.to_string(),
            output: Path::new(out_dir).join(lib_file),
        }
    }

    fn src_obj(&mut self, src: &str) -> (Path,
                                         Path) {
        use std::io::fs::PathExtensions;

        let src = Path::new(src);
        let stem = src.filestem_str().expect(format!("most odd source filename you've got there: `{}`",
                                                     src.display()).as_slice());
        let obj = Path::new(stem).with_extension("o");
        let obj = self.tmp.path().join(obj);
        let obj = if obj.exists() {
            let obj = Path::new(format!("{}{}.o",
                                        stem,
                                        {
                                            let d = self.doubles;
                                            self.doubles += 1;
                                            d
                                        }));
            self.tmp.path().join(obj)
        } else {
            obj
        };
        self.obj_files.push_front(obj.clone());
        (src.clone(), obj)
    }

    fn run(&mut self, cmd: Command) -> &mut Archive {
        run_tool(cmd);
        self
    }
    fn build_cflags() -> Vec<String> {
        let mut a = Vec::new();

        let sdk = get_sdk_root();
        let target = get_nacl_target();

        a.push(format!("-I{}/include", sdk.display()));
        match target {
            Some(Mode::Portable) => {
                a.push(format!("-I{}/include/pnacl", sdk.display()))
            }
            _ => (),
        }
        a
    }

    pub fn cc(&mut self, src: &str, args: &[String]) -> &mut Archive {
        let (src, obj) = self.src_obj(src);

        let mut cmd = Command::new(&self.cc);
        cmd.arg("-c")
            .arg(src)
            .arg("-o")
            .arg(obj)
            .args(args)
            .args(Archive::build_cflags().as_slice());

        self.run(cmd)
    }

    pub fn cxx(&mut self, src: &str, args: &[String]) -> &mut Archive {
        let (src, obj) = self.src_obj(src);

        let mut cmd = Command::new(&self.cxx);
        cmd.arg("-c")
            .arg(src)
            .arg("-o")
            .arg(obj)
            .args(args)
            .args(Archive::build_cflags().as_slice());

        self.run(cmd)
    }

    pub fn archive(mut self) {
        let ar_cmds = "crus";

        let mut cmd = Command::new(&self.ar);
        cmd.arg(ar_cmds)
            .arg(self.output.clone());
        for f in self.obj_files.iter() {
            cmd.arg(f.display().to_string());
        }
        self.run(cmd);

        let out_dir = self.output.dirname_str().unwrap();
        println!("cargo:rustc-flags=-L {} -l {}:static",
                 out_dir, self.libname);
    }
}
