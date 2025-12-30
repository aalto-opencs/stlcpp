# TODO

- fix duplicate declaration error
- make type variables and term variables use a different string newtype to avoid confusion
- add type level syntax. this allows us to write `[a] = List a` and helps with defining custom types
  - Note: this probably requires a subst_ty_all in Term

- Try to get rid of 'b in     syntaxes: &'b [Syntax]

- support fun short-hand
  fun (A) (x : A) (y : A), x + y
  -> fun A, fun x : A, fun y : A, x + y
