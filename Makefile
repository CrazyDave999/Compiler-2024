all: build

build:
	cargo build

run:
	cargo run -- -fsyntax-only