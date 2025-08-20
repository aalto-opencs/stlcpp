// mod common;
// use common::util::*;

// #[test]
// fn test_var() {
//     assert_eq!(var("x").subst("x", id2_()), id2_());
//     assert_eq!(var("y").subst("x", id2_()), var("y"));
// }

// #[test]
// fn test_abs_no_var() {
//     assert_eq!(id2("y").subst("x", id2_()), id2("y"));
// }

// #[test]
// fn test_abs_bound() {
//     assert_eq!(id2_().subst("x", id2_()), id2_());
// }

// #[test]
// fn test_abs_free() {
//     assert_eq!(
//         abs("x", Boolean, var("y")).subst("y", id2_()),
//         abs("x", Boolean, id2_())
//     );
// }

// #[test]
// fn test_app() {
//     let term = app(var("x"), var("y"));
//     let expected = app(id2_(), var("y"));
//     assert_eq!(term.subst("x", id2_()), expected);
// }

// #[test]
// fn test_abs_app_free() {
//     assert_eq!(
//         abs("x", Boolean, app(var("y"), var("y"))).subst("y", id2_()),
//         abs("x", Boolean, app(id2_(), id2_()))
//     );
// }

// #[test]
// fn test_let() {
//     assert_eq!(
//         letin("x", var("y"), id2_()).subst("y", id2_()),
//         letin("x", id2_(), id2_())
//     );

//     assert_eq!(
//         letin("x", id2_(), var("y")).subst("y", id2_()),
//         letin("x", id2_(), id2_())
//     );
// }

// #[test]
// fn test_let_bound() {
//     assert_eq!(
//         letin("x", id2_(), var("x")).subst("x", id2_()),
//         letin("x", id2_(), var("x"))
//     );
// }

// #[test]
// fn test_let_val_t() {
//     assert_eq!(
//         letin("x", var("x"), id2_()).subst("x", id2_()),
//         letin("x", id2_(), id2_())
//     );

//     assert_eq!(
//         letin("x", var("x"), var("x")).subst("x", id2_()),
//         letin("x", id2_(), var("x"))
//     );
// }

// #[test]
// fn test_let_nested() {
//     assert_eq!(
//         letin("x", letin("x", var("x"), id2_()), id2_()).subst("x", id2_()),
//         letin("x", letin("x", id2_(), id2_()), id2_())
//     );

//     assert_eq!(
//         letin("x", var("x"), letin("x", var("x"), var("x"))).subst("x", id2_()),
//         letin("x", id2_(), letin("x", var("x"), var("x")))
//     );

//     assert_eq!(
//         letin("x", app(var("x"), id2("y")), var("x")).subst("x", id2("z")),
//         letin("x", app(id2("z"), id2("y")), var("x"))
//     );
// }

// #[test]
// fn test_ite() {
//     assert_eq!(
//         ite(var("x"), var("y"), var("z"))
//             .subst("x", id2("x"))
//             .subst("y", id2("y"))
//             .subst("z", id2("z")),
//         ite(id2("x"), id2("y"), id2("z"))
//     );
// }

// #[test]
// fn test_add() {
//     // Substitute "x" with Int(42) in (x + y) results in (42 + y).
//     let term = add(var("x"), var("y"));
//     let expected = add(Int(42), var("y"));
//     assert_eq!(term.subst("x", Int(42)), expected);
// }

// #[test]
// fn test_sub() {
//     // Substitute "x" with Int(42) in (y - x) results in (y - 42).
//     let term = sub(var("y"), var("x"));
//     let expected = sub(var("y"), Int(42));
//     assert_eq!(term.subst("x", Int(42)), expected);
// }

// #[test]
// fn test_mul() {
//     // Substitute "x" with Int(42) in (x * y) results in (42 * y).
//     let term = mul(var("x"), var("y"));
//     let expected = mul(Int(42), var("y"));
//     assert_eq!(term.subst("x", Int(42)), expected);
// }

// #[test]
// fn test_subst_eq() {
//     // Substitute "x" with Int(42) in (x == y) results in (42 == y).
//     let term = eq(var("x"), var("y"));
//     let expected = eq(Int(42), var("y"));
//     assert_eq!(term.subst("x", Int(42)), expected);
// }

// #[test]
// fn test_subst_ne() {
//     // Substitute "x" with Int(42) in (x != y) results in (42 != y).
//     let term = ne(var("x"), var("y"));
//     let expected = ne(Int(42), var("y"));
//     assert_eq!(term.subst("x", Int(42)), expected);
// }

// #[test]
// fn test_subst_lt() {
//     // Substitute "x" with Int(42) in (x < y) results in (42 < y).
//     let term = lt(var("x"), var("y"));
//     let expected = lt(Int(42), var("y"));
//     assert_eq!(term.subst("x", Int(42)), expected);
// }

// #[test]
// fn test_subst_le() {
//     // Substitute "x" with Int(42) in (x <= y) results in (42 <= y).
//     let term = le(var("x"), var("y"));
//     let expected = le(Int(42), var("y"));
//     assert_eq!(term.subst("x", Int(42)), expected);
// }

// #[test]
// fn test_subst_gt() {
//     // Substitute "x" with Int(42) in (x > y) results in (42 > y).
//     let term = gt(var("x"), var("y"));
//     let expected = gt(Int(42), var("y"));
//     assert_eq!(term.subst("x", Int(42)), expected);
// }

// #[test]
// fn test_subst_ge() {
//     // Substitute "x" with Int(42) in (x >= y) results in (42 >= y).
//     let term = ge(var("x"), var("y"));
//     let expected = ge(Int(42), var("y"));
//     assert_eq!(term.subst("x", Int(42)), expected);
// }

// #[test]
// fn test_subst_pair() {
//     // Substitute in a pair: subst((x, y), x, 1) = (1, y)
//     let term = pair(var("x"), var("y"));
//     let result = term.subst("x", Int(1));
//     let expected = pair(Int(1), var("y"));
//     assert_eq!(result, expected);
// }

// #[test]
// fn test_subst_nested_pair() {
//     // Substitute in a nested pair: subst((x, (y, z)), y, 2) = (x, (2, z))
//     let term = pair(var("x"), pair(var("y"), var("z")));
//     let result = term.subst("y", Int(2));
//     let expected = pair(var("x"), pair(Int(2), var("z")));
//     assert_eq!(result, expected);
// }

// #[test]
// fn test_subst_fst() {
//     // Substitute in 'fst': subst(fst (x, y), x, 1) = fst (1, y)
//     let term = fst(pair(var("x"), var("y")));
//     let result = term.subst("x", Int(1));
//     let expected = fst(pair(Int(1), var("y")));
//     assert_eq!(result, expected);
// }

// #[test]
// fn test_subst_snd() {
//     // Substitute in 'snd': subst(snd (x, y), y, 2) = snd (x, 2)
//     let term = snd(pair(var("x"), var("y")));
//     let result = term.subst("y", Int(2));
//     let expected = snd(pair(var("x"), Int(2)));
//     assert_eq!(result, expected);
// }

// #[test]
// fn test_subst_pair_in_ite() {
//     // Substitute in pairs in if-then-else expressions:
//     // subst(if True then (x, y) else (a, b), x, 1) = if True then (1, y) else (a, b)
//     let term = ite(True, pair(var("x"), var("y")), pair(var("a"), var("b")));
//     let result = term.subst("x", Int(1));
//     let expected = ite(True, pair(Int(1), var("y")), pair(var("a"), var("b")));
//     assert_eq!(result, expected);
// }

// #[test]
// fn test_subst_pair_function_body() {
//     // Substitute in a function that returns a pair:
//     // subst(fun x : Integer, (x, y), y, 2) = fun x : Integer, (x, 2)
//     let term = abs("x", Integer, pair(var("x"), var("y")));
//     let result = term.subst("y", Int(2));
//     let expected = abs("x", Integer, pair(var("x"), Int(2)));
//     assert_eq!(result, expected);
// }

// #[test]
// fn test_subst_cons() {
//     // Given: Cons(Var("x"), Var("y"))
//     // Substitute "x" with Int(42) should yield: Cons(Int(42), Var("y"))
//     let term = cons(var("x"), var("y"));
//     let result = term.subst("x", Int(42));
//     let expected = cons(Int(42), var("y"));
//     assert_eq!(result, expected);
// }

// #[test]
// fn test_subst_lcase_no_shadow() {
//     // Test LCase where the substitution variable "x" is not equal to head_var or tail_var.
//     // Let:
//     //   t     = Var("x")
//     //   nil_t = Var("x")
//     //   head_var = "a", tail_var = "b"
//     //   cons_t = Var("x")
//     //
//     // Substituting "x" with Int(99) should affect all subterms.
//     let term = lcase(var("x"), var("x"), "a", "b", var("x"));
//     let expected = lcase(Int(99), Int(99), "a", "b", Int(99));
//     assert_eq!(term.subst("x", Int(99)), expected);
// }

// #[test]
// fn test_subst_lcase_with_shadow_head() {
//     // Test LCase where head_var equals the substitution variable.
//     // Let:
//     //   t     = Var("x")
//     //   nil_t = Var("z")  (unchanged because "z" ≠ "x")
//     //   head_var = "x", tail_var = "b"
//     //   cons_t = Var("x")
//     //
//     // When substituting "x" with Int(100), the fields t and nil_t are substituted normally,
//     // but cons_t remains unchanged because the substitution variable "x" equals head_var.
//     let term = lcase(var("x"), var("z"), "x", "b", var("x"));
//     let expected = lcase(Int(100), var("z"), "x", "b", var("x"));
//     assert_eq!(term.subst("x", Int(100)), expected);
// }

// #[test]
// fn test_subst_lcase_with_shadow_tail() {
//     // Test LCase where tail_var equals the substitution variable.
//     // Let:
//     //   t     = Var("z")  (unchanged as it doesn't match "x")
//     //   nil_t = Var("x")
//     //   head_var = "a", tail_var = "x"
//     //   cons_t = Var("x")
//     //
//     // Substituting "x" with Int(50) should substitute nil_t (yielding Int(50))
//     // but leave cons_t unchanged because tail_var equals "x".
//     let term = lcase(var("z"), var("x"), "a", "x", var("x"));
//     let expected = lcase(var("z"), Int(50), "a", "x", var("x"));
//     assert_eq!(term.subst("x", Int(50)), expected);
// }

// #[test]
// fn test_subst_case_no_shadow() {
//     // Test a case expression where the substitution variable "x" is not equal to either branch binder.
//     //
//     // Let:
//     //   scrutinee: Var("x")
//     //   inl branch binder: "a", body: Var("x")
//     //   inr branch binder: "b", body: Var("x")
//     //
//     // Substituting "x" with Int(99) should affect the scrutinee and both branch bodies.
//     let term = case(var("x"), "a", var("x"), "b", var("x"));
//     let expected = case(Int(99), "a", Int(99), "b", Int(99));
//     assert_eq!(term.subst("x", Int(99)), expected);
// }

// #[test]
// fn test_subst_case_with_shadow_inl() {
//     // Test a case expression where the inl branch binder equals the substitution variable.
//     //
//     // Let:
//     //   scrutinee: Var("x")
//     //   inl branch binder: "x", body: Var("x")
//     //   inr branch binder: "b", body: Var("x")
//     //
//     // Substituting "x" with Int(100) should substitute the scrutinee and the inr branch,
//     // but leave the inl branch unchanged because "x" is bound there.
//     let term = case(var("x"), "x", var("x"), "b", var("x"));
//     let expected = case(Int(100), "x", var("x"), "b", Int(100));
//     assert_eq!(term.subst("x", Int(100)), expected);
// }

// #[test]
// fn test_subst_case_with_shadow_inr() {
//     // Test a case expression where the inr branch binder equals the substitution variable.
//     //
//     // Let:
//     //   scrutinee: Var("x")
//     //   inl branch binder: "a", body: Var("x")
//     //   inr branch binder: "x", body: Var("x")
//     //
//     // Substituting "x" with Int(50) should substitute the scrutinee and the inl branch,
//     // but leave the inr branch unchanged because the binder "x" shadows the substitution.
//     let term = case(var("x"), "a", var("x"), "x", var("x"));
//     let expected = case(Int(50), "a", Int(50), "x", var("x"));
//     assert_eq!(term.subst("x", Int(50)), expected);
// }

// #[test]
// fn test_subst_fix() {
//     let term = fix(abs("x", Integer, var("y")));
//     assert_eq!(term.subst("y", Int(1)), fix(abs("x", Integer, Int(1))));
// }
