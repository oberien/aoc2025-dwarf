use std::process::Command;
use tempfile::NamedTempFile;
use gimli::Value;
use crate::compile::compile;
use crate::dwarf_program::DwarfProgram;
use crate::parse::parse;

fn run(code: &str, expected: Value) {
    let items = parse(code);
    let mut program = DwarfProgram::new();
    compile(&mut program, items);
    let result = program.run("test".to_string());
    assert_eq!(result, expected);
}
fn run_gbd(code: &str, expected: &str) {
    let items = parse(code);
    let mut program = DwarfProgram::new();
    compile(&mut program, items);
    let file = NamedTempFile::new().unwrap();
    program.write_to(&file).unwrap();
    let output = Command::new("gdb")
        .arg(file.path())
        .arg("--batch")
        .arg("-ex")
        .arg("p/x test")
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains(expected));
}

#[test]
fn test_rodata() {
    run(r#"
        #rodata testrodata {
            #u64 0x0102030405060708
        }
        #var test {
            addr testrodata
            deref
        }
    "#, Value::Generic(0x0102030405060708));
    run(r#"
        #rodata testrodata {
            #u64 0
        }
        #var test {
            addr testrodata.len
            deref
        }
    "#, Value::Generic(8));
}

#[test]
fn test_debug_is_noop() {
    run(r#"
        #var test {
            constu 42
            #debug
        }
    "#, Value::Generic(42));
}

#[test]
fn test_gdb_bra_bug() {
    // this code failed in gdb before <https://sourceware.org/git/?p=binutils-gdb.git;a=commit;h=cc27559a20f4238a0f3fb31c02dbd2c9fecaed65>
    run(r#"
        #var test {
            constu 42
            constu 1
            bra .end
        .end:
        }
    "#, Value::Generic(42));
    // this code worked before the patch
    run(r#"
        #var test {
            constu 0
            constu 1
            bra .end
            skip .end2
        .end:
            nop
        .end2:
        }
    "#, Value::Generic(0));
}

#[test]
fn test_if_else() {
    run(r#"
        #var test {
            #if (constu 5, constu 1, plus) < (constu 6) {
                constu 1337
            } #else #if (constu 5) == (constu 3) {
                constu 42
            } #else {
                constu 21
            }
        }
    "#, Value::Generic(21));
}

#[test]
fn test_const_type() {
    run(r#"
        #var test {
            const_type $u8, #u8 42
        }
    "#, Value::U8(42));
    run(r#"
        #var test {
            const_type $i64, #i64 -1337
        }
    "#, Value::I64(-1337));
}
#[test]
#[should_panic]
fn test_const_type_invalid_length_should_panic() {
    run(r#"
        #var test {
            const_type $u16, #u8 42
        }
    "#, Value::Generic(0xbad));
}

#[test]
fn test_struct() {
    run(r#"
        #type $Foo {
            foo: $u16,
            bar: $Bar,
        }
        #type $Bar {
            baz: $u8,
        }

        #var test {
            #create $Foo {
                foo: 0x1100,
                bar: $Bar {
                    baz: 0x42,
                },
            }
            dup
            #access $Foo.foo
            pick 1
            #access $Foo.bar.baz
            plus
            #set $Foo.foo
            #access $Foo.foo
        }
    "#, Value::Generic(0x1142))
}

#[test]
fn test_gdb() {
    run_gbd(r#"
        #var test {
            constu 0x1337
        }
    "#, "$1 = 0x1337")
}
