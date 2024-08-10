# Mx Compiler 2024

使用`rustc 1.77.2`，`pest 2.7.11`

## Usage

- 评测模式 
```./testcases/se
  ./testcases/sema/scripts/test_all.bash './target/debug/compiler-2024 -fsyntax-only' ./testcases/sema/
```
- 简单调试模式
```
    ./target/debug/compiler-2024 -fsyntax-test <src>
```
- 详细调试模式
```
    ./target/debug/compiler-2024 -debug <src>
```