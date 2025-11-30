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
fn run_gdb(code: &str, expected: &str) {
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
    assert!(stdout.contains(expected), "stdout doesn't contain {expected:?}:\ncode:\n {code}\n\nstdout:\n{stdout}");
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
    run_gdb(r#"
        #var test {
            constu 0x1337
        }
    "#, "$1 = 0x1337")
}

#[test]
fn test_gdb_convert() {
    let int_tests = [
        // convert to smaller values
        ("constu 0x1337\nconvert $u8", "0x37"),
        ("const_type $u16, #u16 0x1337\nconvert $u8", "0x37"),
        ("const_type $i16, #i16 -0x1337\nconvert $u8", "0x37"),
        ("const_type $Foo, #u8 0x11, 0x22, 0x33\nconvert $u8", "0x11"),
        // convert to larger values
        ("const_type $u8, #u8 0x11\nconvert $u16", "0x11"),
        ("const_type $u8, #u8 0x11\nconvert $i16", "0x11"),
        ("const_type $i8, #i8 -0x11\nconvert $i16", "0xffef"),
        ("const_type $i8, #i8 -0x11\nconvert $u16", "0xffef"),
        ("const_type $i8, #i8 -0x11\nconvert $Foo", "0xffffef"),
    ];
    for (code, result) in int_tests {
        run_gdb(&format!(r#"
            #type $Foo {{
                a: $u8,
                b: $u8,
                c: $u8,
            }}
            #var test {{
                {code}
            }}"#), result);
    }

    macro_rules! float_test {
        ($to_test:expr, $typ:literal, $gdb_actual:expr, $rust_actual:expr, $rust_expected:expr) => {
            ($to_test, $typ, $gdb_actual as u16, $rust_actual as u16, $rust_expected as u16)
        }
    }
    let float_tests = [
        // (to-test, type, gdb-actual, rust-actual, rust-expected)
        // inf: 0x7ff0000000000000
        float_test!(f64::INFINITY, "$u16", u16::MAX, f64::INFINITY as u16, u16::MAX),
        float_test!(f64::INFINITY, "$i16", -1i16, f64::INFINITY as i16, i16::MAX), // BUG in gdb?
        // -inf: 0xfff0000000000000
        float_test!(f64::NEG_INFINITY, "$u16", 0u16, f64::NEG_INFINITY as u16, 0u16),
        float_test!(f64::NEG_INFINITY, "$i16", 0i16, f64::NEG_INFINITY as i16, i16::MIN), // BUG in gdb?
        // nan: 0x7ff8000000000000
        float_test!(f64::NAN, "$u16", u16::MAX, f64::NAN as u16, 0u16), // BUG in gdb?
        float_test!(f64::NAN, "$i16", -1i16, f64::NAN as i16, 0i16), // BUG in gdb?
        // max: 0x7fefffffffffffff
        float_test!(f64::MAX, "$u16", u16::MAX, f64::MAX as u16, u16::MAX),
        float_test!(f64::MAX, "$i16", -1i16, f64::MAX as i16, i16::MAX), // BUG in gdb?
        // min: 0xffefffffffffffff
        float_test!(f64::MIN, "$u16", 0u16, f64::MIN as u16, 0u16),
        float_test!(f64::MIN, "$i16", 0i16, f64::MIN as i16, i16::MIN), // BUG in gdb?
        // -0.: 0x8000000000000000
        float_test!(-0f64, "$u16", 0u16, -0f64 as u16, 0u16),
        float_test!(-0f64, "$i16", 0i16, -0f64 as i16, 0i16),
        // 0.: 0x0000000000000000
        float_test!(0f64, "$u16", 0u16, 0f64 as u16, 0u16),
        float_test!(0f64, "$i16", 0i16, 0f64 as i16, 0i16),
    ];
    for (value, typ, gdb_actual, rust_actual, rust_expected) in float_tests {
        let value = value.to_bits();
        assert_eq!(rust_actual, rust_expected);
        run_gdb(&format!(r#"
            #var test {{
                const_type $f64, #u64 {value:#x}
                convert {typ}
            }}
        "#), &format!("{gdb_actual:#x}"));
    }
}
