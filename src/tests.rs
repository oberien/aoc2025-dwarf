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
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stdout.contains(expected) || stderr.contains(expected), "stdout/stderr don't contain {expected:?}:\ncode:\n {code}\n\nstdout:\n{stdout}\n-----\n\nstderr:\n{stderr}\n-----");
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
            #get $Foo.foo
            pick 1
            #get $Foo.bar.baz
            plus
            #set $Foo.foo
            #get $Foo.foo
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
        "#), &format!("{gdb_actual:#x}\n"));
    }
}

#[test]
fn test_gdb_abs() {
    let tests = [
        // "positive" generic type
        ("constu 0x1337\nabs", "0x1337\n"),
        // "negative" generic type
        ("constu 0xffff_ffff_ffff_ffff\nabs", "0x1\n"),
    ];
    for (code, result) in tests {
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
}
#[test]
fn test_gdb_neg() {
    let tests = [
        // generic type
        ("constu 0x1337\nneg", "0xffffffffffffecc9\n"),
        // unsigned "positive" types that fit into the target type
        ("const_type $u8, #u8 0x1\nneg", "0xff\n"),
        ("const_type $u16, #u16 0x1\nneg", "0xffff\n"),
        ("const_type $u32, #u32 0x1\nneg", "0xffffffff\n"),
        ("const_type $u64, #u64 0x1\nneg", "0xffffffffffffffff\n"),
        ("const_type $Foo, #u8 0x01, 0x00, 0x00\nneg", "0xffffff\n"),
        // unsigned "negative" types that fit into the target type
        ("const_type $u8, #u8 0x81\nneg", "0x7f\n"),
        ("const_type $u16, #u16 0x8001\nneg", "0x7fff\n"),
        ("const_type $u32, #u32 0x80000001\nneg", "0x7fffffff\n"),
        ("const_type $u64, #u64 0x8000_0000_0000_0001\nneg", "0x7fffffffffffffff\n"),
        ("const_type $Foo, #u8 0x01, 0x00, 0x80\nneg", "0x7fffff\n"),
        // unsigned types that _don't_ fit into the target type
        // -> undefined according to DWARF spec
        ("const_type $u8, #u8 0x80\nneg", "0x80\n"),
        ("const_type $u16, #u16 0x8000\nneg", "0x8000\n"),
        ("const_type $u32, #u32 0x80000000\nneg", "0x80000000\n"),
        ("const_type $u64, #u64 0x8000_0000_0000_0000\nneg", "0x8000000000000000\n"),
        ("const_type $Foo, #u8 0x00, 0x00, 0x80\nneg", "0x800000\n"),
        // check that the type stays the original type after neg
        ("const_type $u8, #u8 0x1\nneg\nconst_type $u8, #u8 2\nplus", "0x1\n"),
        ("const_type $u8, #u8 0x1\nneg\nconst_type $i8, #i8 2\nplus", "Incompatible types on DWARF stack\n"),
        ("const_type $i8, #i8 0x1\nneg\nconst_type $u8, #i8 2\nplus", "Incompatible types on DWARF stack\n"),
        ("const_type $i8, #i8 0x1\nneg\nconst_type $i8, #u8 2\nplus", "0x1\n"),
    ];
    for (code, result) in tests {
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
}

#[test]
fn test_gdb_plus() {
    let tests = [
        // "unsigned" generic type
        ("constu 0x1\nconstu 0x1\nplus", "0x2\n"),
        // "signed" generic type
        ("consts 0x1\nconsts 0x1\nplus", "0x2\n"),
        // mix signed and unsigned generic type
        ("constu 0x1\nconsts 0x1\nplus", "0x2\n"),
        // mix generic and typed integers
        ("constu 0x1\nconst_type $u8, #u8 0x1\nplus", "Incompatible types on DWARF stack\n"),
        ("constu 0x1\nconst_type $u16, #u16 0x1\nplus", "Incompatible types on DWARF stack\n"),
        ("constu 0x1\nconst_type $u32, #u32 0x1\nplus", "Incompatible types on DWARF stack\n"),
        ("constu 0x1\nconst_type $u64, #u64 0x1\nplus", "Incompatible types on DWARF stack\n"),
        // mix signed and unsigned of same size
        ("const_type $u8, #u8 0x1\nconst_type $i8, #i8 0x1\nplus", "Incompatible types on DWARF stack\n"),
        ("const_type $u16, #u16 0x1\nconst_type $i16, #i16 0x1\nplus", "Incompatible types on DWARF stack\n"),
        ("const_type $u32, #u32 0x1\nconst_type $i32, #i32 0x1\nplus", "Incompatible types on DWARF stack\n"),
        ("const_type $u64, #u64 0x1\nconst_type $i64, #i64 0x1\nplus", "Incompatible types on DWARF stack\n"),
        // mix unsigned of different sizes
        ("const_type $u8, #u8 0x1\nconst_type $u16, #u16 0x1\nplus", "Incompatible types on DWARF stack\n"),
        // mix signed of different sizes
        ("const_type $i8, #i8 0x1\nconst_type $i16, #i16 0x1\nplus", "Incompatible types on DWARF stack\n"),
    ];
    for (code, result) in tests {
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
}

#[test]
fn test_gdb_not() {
    let tests = [
        ("const_type $f64, #u64 0x3ff0000000000000\nnot", "integral type expected in DWARF expression\n"),
        ("const_type $u8, #u8 0b0000_1111\nnot", "0xf0\n"),
        ("const_type $Foo, #u8 0x0f, 0xf0, 0x00\nnot", "0xff0ff0\n"),
    ];
    for (code, result) in tests {
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
}

#[test]
fn test_gdb_shr() {
    let tests = [
        ("const_type $f64, #u64 0x3ff0000000000000\nconstu 2\nshr", "Incompatible types on DWARF stack\n"),
        ("const_type $u8, #u8 0x10\nconstu 2\nshr", "Incompatible types on DWARF stack\n"),
        ("const_type $u8, #u8 0x10\nconst_type $u8, #u8 2\nshr", "0x4\n"),
        ("const_type $i8, #i8 0x10\nconst_type $i8, #i8 2\nshr", "0x4\n"),
        ("const_type $i8, #i8 -0x10\nconst_type $i8, #i8 -2\nshr", "0x0\n"),
        ("const_type $i8, #i8 -0x10\nconst_type $i8, #i8 2\nshr", "0x3c\n"),
        ("const_type $Foo, #u8 0x00, 0x00, 0x10\nconst_type $Foo, #u8 10, 0, 0\nshr", "0x400\n"),
    ];
    for (code, result) in tests {
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
}

#[test]
fn test_gdb_shra() {
    let tests = [
        ("const_type $f64, #u64 0x3ff0000000000000\nconstu 2\nshra", "Incompatible types on DWARF stack\n"),
        ("const_type $u8, #u8 0x10\nconstu 2\nshra", "Incompatible types on DWARF stack\n"),
        ("const_type $u8, #u8 0x10\nconst_type $u8, #u8 2\nshra", "0x4\n"),
        ("const_type $u8, #u8 0xf0\nconst_type $u8, #u8 2\nshra", "0xfc\n"),
        ("const_type $i8, #i8 0x10\nconst_type $i8, #i8 2\nshra", "0x4\n"),
        ("const_type $i8, #i8 -0x10\nconst_type $i8, #i8 -2\nshra", "0xff\n"),
        ("const_type $i8, #i8 -0x10\nconst_type $i8, #i8 2\nshra", "0xfc\n"),
        ("const_type $i8, #i8 0x10\nconst_type $i8, #i8 100\nshra", "0x0\n"),
        ("const_type $i8, #i8 -0x10\nconst_type $i8, #i8 100\nshra", "0xff\n"),
        ("const_type $Foo, #u8 0x00, 0x00, 0x10\nconst_type $Foo, #u8 10, 0, 0\nshra", "no signed variant found for type, while evaluating DWARF expression\n"),
    ];
    for (code, result) in tests {
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
}

#[test]
fn test_arrays_u8() {
    let tests = [
        // init, code, expected
        (0, "#get $ArrayU8.data[0]", 0),
        (42, "#get $ArrayU8.data[2]", 42),
        (42, "constu 1\n#getindex $ArrayU8.data", 42),
        (42, "constu 21\n#set $ArrayU8.data[0]\n#get $ArrayU8.data[0]", 21),
        (42, "constu 21\n#set $ArrayU8.data[1]\n#get $ArrayU8.data[0]", 42),
        (42, "constu 21\nconstu 2\n#setindex $ArrayU8.data\n#get $ArrayU8.data[2]", 21),
        (42, "constu 21\nconstu 2\n#setindex $ArrayU8.data\n#get $ArrayU8.data[1]", 42),
    ];
    for (init, code, expected) in tests {
        run(&format!(r#"
            #type $ArrayU8 {{
                data: [$u8; 3],
            }}

            #var test {{
                #create $ArrayU8 {{
                    data: [{init}; 3],
                }}
                {code}
            }}
        "#), Value::Generic(expected));
    }
}

#[test]
fn test_arrays_u16() {
    let tests = [
        // init, code, expected
        (0, "#get $ArrayU16.data[0]", 0),
        (0x1337, "#get $ArrayU16.data[2]", 0x1337),
        (0x1337, "constu 1\n#getindex $ArrayU16.data", 0x1337),
        (0x1337, "constu 0x42\n#set $ArrayU16.data[0]\n#get $ArrayU16.data[0]", 0x42),
        (0x1337, "constu 0x42\n#set $ArrayU16.data[1]\n#get $ArrayU16.data[0]", 0x1337),
        (0x1337, "constu 0x42\nconstu 2\n#setindex $ArrayU16.data\n#get $ArrayU16.data[2]", 0x42),
        (0x1337, "constu 0x42\nconstu 2\n#setindex $ArrayU16.data\n#get $ArrayU16.data[1]", 0x1337),
    ];
    for (init, code, expected) in tests {
        run(&format!(r#"
            #type $ArrayU16 {{
                data: [$u16; 3],
            }}

            #var test {{
                #create $ArrayU16 {{
                    data: [{init}; 3],
                }}
                {code}
            }}
        "#), Value::Generic(expected));
    }
}
