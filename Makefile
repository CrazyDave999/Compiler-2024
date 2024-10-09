all: build

PROJECT_NAME = compiler-2024

build:
	cargo build

run:
	ulimit -s 131072; \
	./target/debug/$(PROJECT_NAME)

debug:
	./target/debug/$(PROJECT_NAME) -debug ./test

sema-test-all:
	./testcases/sema/scripts/test_all.bash './target/debug/compiler-2024' ./testcases/sema

llvm-test:
	./target/debug/compiler-2024 -emit-test ./test
	clang -S -O0 test.ll builtin.ll --target=riscv32-unknown-elf
	#ravel --input-file=test.in --output-file=test.out test.s builtin.s
	reimu -i=test.in -o test.out -s 256k

llvm-test-all:
	./testcases/codegen/scripts/test_llvm_ir_all.bash './target/debug/compiler-2024 -emit-llvm' ./testcases/codegen ./builtin.ll

asm-test:
	ulimit -s 131072; \
	./target/debug/compiler-2024 -test ./test; \
	#ravel --input-file=test.in --output-file=test.out test.s builtin.s; \
	reimu -i test.in -o test.out
asm-test-all:
	./testcases/codegen/scripts/test_asm_all.bash './target/debug/compiler-2024' ./testcases/codegen ./builtin.s