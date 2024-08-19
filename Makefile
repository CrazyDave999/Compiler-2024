all: build

PROJECT_NAME = compiler-2024

build:
	cargo build

run:
	./target/debug/$(PROJECT_NAME) -fsyntax-only

llvm-test:
	./target/debug/compiler-2024 -emit-test ./test
	clang -S -O0 test.ll builtin.ll --target=riscv32-unknown-elf
	ravel --input-file=test.in --output-file=test.out test.s builtin.s

llvm-test-all:
	./testcases/codegen/scripts/test_llvm_ir_all.bash './target/debug/compiler-2024 -emit-llvm' ./testcases/codegen ./builtin.ll

asm-test:
	./target/debug/compiler-2024 -test ./test