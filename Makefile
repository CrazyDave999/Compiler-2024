all: build

PROJECT_NAME = compiler-2024

build:
	cargo build

run:
	./target/debug/$(PROJECT_NAME) -fsyntax-only