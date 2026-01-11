# TODO

- fix duplicate declaration error
- make type variables and term variables use a different string newtype to avoid confusion
- add type level syntax. this allows us to write `[a] = List a` and helps with defining custom types
  - Note: this probably requires a subst_ty_all in Term

- Try to get rid of 'b in     syntaxes: &'b [Syntax]

- support fun short-hand
  fun (A) (x : A) (y : A), x + y
  -> fun A, fun x : A, fun y : A, x + y

## Adding cuts

Cuts help the parser to produce a more helpful error message when a critical parsing failure occurs.
This is particularly helpful for parsers that are used inside an `alt`.

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
