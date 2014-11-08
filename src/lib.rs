use std::collections::RingBuf;
use std::io::{Command, TempDir};
use std::os::getenv;

enum Mode {
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
        let mode = match getenv("TARGET").unwrap().as_slice() {
            "le32-unknown-nacl" => Portable,
            "i686-unknown-nacl" => Native("i686"),
            "x86_64-unknown-nacl" => Native("x86_64"),
            "arm-unknown-nacl" => Native("arm"),
            _ => panic!("todo"),
        };

        let pepper = match getenv("NACL_SDK_ROOT") {
            None => panic!("Please set NACL_SDK_ROOT to your local pepper sdk"),
            Some(p) => Path::new(p),
        };

        let pepper = pepper.join("toolchain");

        let (cc, cxx, ar, ranlib) = match mode {
            Portable => {
                let cc     = "pnacl-clang";
                let cxx    = "pnacl-clang++";
                let ar     = "pnacl-ar";
                let ranlib = "pnacl-ranlib";

                let pepper = pepper.join([get_platform_str(),"_pnacl"].concat());
                let pepper = pepper.join("bin");
                (pepper.join(cc),
                 pepper.join(cxx),
                 pepper.join(ar),
                 pepper.join(ranlib))
            }
            Native(arch) if arch == "i686" || arch == "x86_64" => {
                let cc     = [arch, "-nacl-gcc"].concat();
                let cxx    = [arch, "-nacl-g++"].concat();
                let ar     = [arch, "-nacl-ar"].concat();
                let ranlib = [arch, "-nacl-ranlib"].concat();

                let pepper = pepper
                    .join_many([[get_platform_str(), "_x86_newlib"].concat(),
                                "bin".to_string()]);
                (pepper.join(cc),
                 pepper.join(cxx),
                 pepper.join(ar),
                 pepper.join(ranlib))
            }
            Native("arm") => {
                let cc     = "arm-nacl-gcc";
                let cxx    = "arm-nacl-g++";
                let ar     = "arm-nacl-ar";
                let ranlib = "arm-nacl-ranlib";

                let pepper = pepper
                    .join_many([[get_platform_str(), "_arm_newlib"].concat(),
                                "bin".to_string()]);
                (pepper.join(cc),
                 pepper.join(cxx),
                 pepper.join(ar),
                 pepper.join(ranlib))
            }
            Native(_) => unreachable!(),
        };
        let out_dir = getenv("OUT_DIR").unwrap();

        Archive {
            cc:     cc,
            cxx:    cxx,
            ar:     ar,
            ranlib: ranlib,

            tmp: TempDir::new(["lib", out_stem, "-objs"].concat().as_slice()).unwrap(),
            doubles: 1,
            obj_files: RingBuf::new(),
            libname: out_stem.to_string(),
            output: Path::new(out_dir).join(["lib", out_stem, ".a"].concat()),
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

    fn run(&mut self, mut cmd: Command) -> &mut Archive {
        assert!(cmd.status().unwrap().success());
        self
    }

    pub fn cc(&mut self, src: &str, args: &[String]) -> &mut Archive {
        let (src, obj) = self.src_obj(src);

        let mut cmd = Command::new(&self.cc);
        cmd.arg("-c")
            .arg(src)
            .arg("-o")
            .arg(obj)
            .args(args);

        self.run(cmd)
    }

    pub fn cxx(&mut self, src: &str, args: &[String]) -> &mut Archive {
        let (src, obj) = self.src_obj(src);

        let mut cmd = Command::new(&self.cxx);
        cmd.arg("-c")
            .arg(src)
            .arg("-o")
            .arg(obj)
            .args(args);

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
