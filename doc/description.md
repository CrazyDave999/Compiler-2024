# Week 4

Target: Use `Rust` programming language to implement a `Mx`-to-`RISCV32I` compiler. Since it seems that the support
of `Antlr` for `Rust` isn't good, I'm considering use `Rust`+`Pest`. Now I'm trying to implement the `parser.pest` to
specify the parser rule for `MX`.

My comprehension to the project at the moment:

- Firstly change the source code into Abstract Semantic tree(AST) with `Rust` and `Pest`. This is the front-end part of
  the project.
- Traverse on the AST in some way, finish the semantic check, and produce Intermediate Representation(IR). Of course, we
  can design the IR on our own, and also we can just obey the LLVM IR format and protocol.
- Finally, we optimize the IR codes we generate and implement the register allocating algorithm, associating with the
  graph-coloring problem. Then we generate the final `RISCV32I` code.

Some questions:

- How should I deal with the OOP part in `MX` language? Should I implement a table-like structure to imitate the
  behavior of classes?
- I am still not clear about how exactly can I produce the IR through traversing the AST nodes. It seems that I should
  design format string patterns for all possible conditions and print them directly.
- I'm not familiar with `Pest`!

# Week 6