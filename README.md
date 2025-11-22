# Advent of Code 2025 using DWARF Expressions

DWARF is the binary debug symbol format used by default on Linux and MacOS.
It's usually embedded directly into a binary built with debug symbols enabled.
To print a variable from a debugger, the debugger needs to know how
to calculate the address of the value of the variable.
In the easiest case, it's just an address.
In more complex cases, e.g. for struct fields, pointer paths are required.
To be able to support arbitrary cases of all languages, DWARF decided to
support DWARF expressions for address/value-calculation.

DWARF expressions are a stack machine with 166 instructions.
It's possible to read the top 256 stack elements.
It's only possible modify the top 3 stack elements.
A stack element can have up to 256 bytes (however not all implementations
allow this).

Gdb <=16.3 has a bug in their DWARF expression evaluator, which is fixed in
<https://sourceware.org/git/?p=binutils-gdb.git;a=commit;h=cc27559a20f4238a0f3fb31c02dbd2c9fecaed65>.
If your gdb is <=16.3, you'll need to build the latest version yourself to
be able to run the code.

### How to run

* compile a file: `cargo run -- foo.dwasm`
* compile all files recursively (effectively concatenate all files; then compile): `cargo run`
* run an object-file's `day02` using gdb: `gdb foo.o --batch -ex "p day02"`

### Useful commands for debugging

* `readelf -a foo.o`: show all
* `readelf -e foo.o`: show sections
* `readelf -r foo.o`: show relocations
* `readelf -s foo.o`: show symbols
* `readelf -w foo.o`: show dwarf DIEs
* `objdump -W foo.o`: show dwarf DIEs
* `objdump -sj .debug_info -j .rodata -j .debug_abbrev foo.o`: hexdump of sections

### Notes:

* lldb doesn't work as it requires a running process for `DW_OP_deref` to work
