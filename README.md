# STLC++

## TODO

- fix duplicate declaration error
- make type variables and term variables use a different string newtype to avoid confusion
- add type level syntax. this allows us to write `[a] = List a` and helps with defining custom types
  - Note: this probably requires a subst_ty_all in Term

- Try to get rid of 'b in     syntaxes: &'b [Syntax]

- support fun short-hand
  fun (A) (x : A) (y : A), x + y
  -> fun A, fun x : A, fun y : A, x + y

## Type shadowing

Question: How to deal with type shadowing? E.g. `(fun A, fun B, fun x : A, x) (forall B, B -> B)` reduces into a term with type shadowing, but it should be sound
Answer: Types are De Bruijn indexed but retain human-readable names in order to format them nicely. During formatting, shadowed types automatically get a subscript `_1`, `_2` etc if necessary.

### Namespaces

Naming convention:
- `TypeName.func1`
- `A.B.C.func`
- `A.b`

#### Gleam style namespaces / modules
```stlc
int.to_bool :: Int -> Bool
```

## Known issues

- Parser is not very robust
    - declarations must be separated by exactly one empty line

- The following fails to parse

```
reverse : [Integer] -> [Integer]
reverse = fun xs : [Integer],
    lcase xs of
    | nil => nil Integer
    | cons x xs => append (reverse xs) (cons x (nil Integer))
```

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
