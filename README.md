# 𝕊𝕋𝕃ℂ++

## Building from Source

### Nix

If you have [Nix](https://nixos.org) installed, you can build $stlcpp$ with just a single command

```
nix-build -I stlcpp=https://github.com/aalto-opencs/stlcpp/archive/main.tar.gz -E "import <stlcpp> {}" -A package
```

You can then find the `stlcpp` binary in `result/bin/stlcpp`.

### Rust

Clone [the repository](https://github.com/aalto-opencs/stlcpp) and run

```
cargo build
```

## Archictecture

### Tokens, terms and types

Tokens, terms and types are all separate objects in the codebase:
- Tokens are split into two categories: term tokens and type tokens.
  - Term tokens have a type state of `Surface` and `Desugared`, where `Surface` tokens may contain custom syntax/operators, but `Desugared` may not.
- (Core) terms (from `src/term.rs`) are what are ultimately evaluated
- Types (from `src/type/named_type.rs`) are the result of type checking desugared term tokens.

## Type shadowing

Question: How to deal with type shadowing? E.g. `(fun A, fun B, fun x : A, x) (forall B, B -> B)` reduces into a term with type shadowing, but it should be sound
Answer: Types are De Bruijn indexed but retain human-readable names in order to format them nicely. During formatting, shadowed types automatically get a subscript `_1`, `_2` etc if necessary.

## Namespaces

### Gleam style namespaces / modules
```stlc
int.to_bool :: Int -> Bool
```

## Known issues

- Parser is not very robust
    - declarations must be separated by exactly one empty line

## Adding cuts

For example, the following error when parsing `cons`

```
examples/simple.stlc λ sum (cons 1 nil Integer)
Parsing Error: Base { location: " (cons 1 nil Integer)", kind: Expected(Eof) }
```

is improved if we add the following `cut`:

```
tag("cons"), // If we see a cons but fail parsing the following, it's unrecoverable
cut((
    multispace1,
    parse_term_primary,
    multispace1,
    parse_term_primary,
)),
```

Now the error is

```
examples/simple.stlc λ sum (cons 1 nil Integer)
Parsing Failure: Stack { base: Alt([Base { location: "nil Integer)", kind: Expected(Char('(')) }, Base { location: "nil Integer)", kind: Kind(Verify) }, Base { location: "nil Integer)", kind: Expected(Digit) }, Base { location: "nil Integer)", kind: Kind(Tag) }, Base { location: "nil Integer)", kind: Kind(Tag) }, Base { location: "nil Integer)", kind: Expected(Char('(')) }, Base { location: "nil Integer)", kind: Expected(Char('\'')) }, Base { location: "nil Integer)", kind: Expected(Char('"')) }]), contexts: [("cons 1 nil Integer)", Context("parse_term")), ("sum (cons 1 nil Integer)", Context("parse_term"))] }
```

## Concrete example why context must not have type shadowing

```
λ fun Y, (fun X, fun x : X, fun Y, x) Y
fun Y : Type, fun X : Type, fun x : X, fun Y : Type, x Y
  :: forall Y, Y -> forall Y, Y       <- this Y should not be capture by the inner forall
λ fun Y, (fun X, fun x : X, fun Y, x) Int
fun Y : Type, fun X : Type, fun x : X, fun Y : Type, x Int
  :: forall Y, Int -> forall Y, Int
λ (fun Y, (fun X, fun x : X, fun Y, x) Y) Int
fun x : Y, fun Y : Type, x
  :: Int -> forall Y, Y
λ (fun X, fun x : X, fun Y, x) Int
fun x : Int, fun Y : Type, x
  :: Int -> forall Y, Int
λ (fun X, fun x : X, fun Y, x) Int 5 Char
5 :: Int
```

Now let's examine the one where the variable Y is captured by the inner type abstraction.
We have formed `forall Y, Y` which is the bottom type in Church encoding.
Normally this is achievable only via an infinite loop, but the following demonstrates that we actually get a type-preservation bug.

```
λ (fun Y, (fun X, fun x : X, fun Y, x) Y) Int
fun x : Y, fun Y : Type, x
  :: Int -> forall Y, Y
λ (fun Y, (fun X, fun x : X, fun Y, x) Y) Int 5
fun Y : Type, 5
  :: forall Y, Y
λ (fun Y, (fun X, fun x : X, fun Y, x) Y) Int 5 Char
5 :: Char
```

## Development

### Testing

Rust tests

```
cargo test
```

Integration E2E tests

```
nix-build -A tests
```

### Web UI

#### Build

```
cargo watch -i .gitignore -i "pkg/*" -s "wasm-pack build --target web"
# or
wasm-pack build --target web
```

#### Serve

```
python playground/server.py
```
