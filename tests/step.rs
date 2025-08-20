// mod common;
// use common::util::*;

// #[test]
// fn test_app_abs() {
//     assert_eq!(app(id2_(), id2_()).step(), id2_());
//     assert_eq!(app(id2_(), tru()).step(), tru());
// }

// #[test]
// fn test_app2() {
//     assert_eq!(app(id2_(), app(id2_(), tru())).step(), app(id2_(), tru()));
// }

// #[test]
// fn test_app1() {
//     assert_eq!(app(app(id2_(), tru()), fals()).step(), app(tru(), fals()));
// }

// #[test]
// #[should_panic(expected = "cannot evaluate a variable")]
// fn test_step_on_variable_panics() {
//     let _ = var("x").step();
// }

// #[test]
// #[should_panic(expected = "cannot step a value")]
// fn test_step_on_abstraction_panics() {
//     let _ = id2_().step();
// }

// #[test]
// #[should_panic(expected = "cannot step a value")]
// fn test_step_on_bool_panics() {
//     let _ = True.step();
// }

// #[test]
// fn test_complex() {
//     // LHS is not a value, but is reducible with AppAbs, so App1 is applied
//     //   ((abs x, abs y, x) (abs z, z)) (abs w, w) --> (abs y, (abs z, z)) (abs w, w)
//     let term = app(app(tru(), id2("z")), id2("w"));
//     let expected = app(abs("y", Boolean, id2("z")), id2("w"));

//     let step1 = term.step();
//     assert_eq!(step1, expected);

//     // As both applicands are value, AppAbs does the substitution
//     //   (abs y, (abs z, z)) (abs w, w) --> abs z, z
//     let step2 = step1.step();
//     let expected = id2("z");
//     assert_eq!(step2, expected);
// }

// #[test]
// fn test_let() {
//     let term = letin("x", id2_(), var("x"));
//     let expected = id2_();
//     // Since the bound value is a value (an abstraction), `step` should immediately
//     // perform the substitution, replacing x in the body.
//     assert_eq!(term.step(), expected);
// }

// #[test]
// fn test_let1() {
//     let term = letin("x", app(id2_(), id2_()), var("x"));
//     // In one step, since the value is not a value, the let should reduce its val_t.
//     let expected = letin("x", id2_(), var("x"));
//     assert_eq!(term.step(), expected);
// }

// #[test]
// fn test_let_substitution_in_body() {
//     // Test that substitution happens in the body.
//     // let x = id2_ in (x id2_)  should substitute x with id2_ in the body.
//     let term = letin("x", id2_(), app(var("x"), id2_()));
//     // Since id2_ is a value, substitution is performed:
//     // The body (x id2_) becomes (id2_ id2_).
//     let expected = app(id2_(), id2_());
//     assert_eq!(term.step(), expected);
// }

// #[test]
// fn test_let_shadowing() {
//     let inner = letin("x", tru(), var("x"));
//     let term = letin("x", id2("z"), inner.clone());
//     assert_eq!(term.step(), inner);
// }

// #[test]
// fn test_let_nested() {
//     let inner = letin("y", tru(), var("x"));
//     let term = letin("x", id2_(), inner.clone());

//     let step1 = term.step();
//     assert_eq!(step1, letin("y", tru(), id2_()));

//     let step2 = step1.step();
//     assert_eq!(step2, id2_());
// }

// #[test]
// fn test_let_complex() {
//     let inner = letin("y", tru(), app(var("y"), var("x")));
//     let term = letin("x", app(id2_(), id2("z")), inner.clone());

//     let step1 = term.step();
//     assert_eq!(step1, letin("x", id2("z"), inner.clone()));

//     let step2 = step1.step();
//     assert_eq!(step2, letin("y", tru(), app(var("y"), id2("z"))));

//     let step3 = step2.step();
//     assert_eq!(step3, app(tru(), id2("z")));
// }

// #[test]
// fn test_let_abs() {
//     let term = letin("x", id2_(), abs("y", Boolean, var("x")));
//     let expected = abs("y", Boolean, id2_());
//     assert_eq!(term.step(), expected);
// }

// #[test]
// fn test_if_true() {
//     assert_eq!(ite(True, id2("x"), id2("y")).step(), id2("x"));
// }

// #[test]
// fn test_if_false() {
//     assert_eq!(ite(False, id2("x"), id2("y")).step(), id2("y"));
// }

// #[test]
// fn test_if() {
//     assert_eq!(
//         ite(app(id2("x"), True), id2("x"), id2("y")).step(),
//         ite(True, id2("x"), id2("y"))
//     );
// }

// #[test]
// fn test_step_app_both_values() {
//     // t1 is an abstraction: fun x: Integer, x+Int(1)
//     // t2 is Int(2)
//     // Expected: substitution in the body: Add(Var("x"), Int(1)) where x is replaced by Int(2).
//     let t1 = abs("x", Integer, add(var("x"), Int(1)));
//     let t2 = Int(2);
//     let app_term = app(t1, t2);
//     let expected = add(Int(2), Int(1));
//     assert_eq!(app_term.step(), expected);
// }

// mod add {
//     use super::*;

//     #[test]
//     fn test_step_add_left_not_value() {
//         // Left operand is not a value.
//         let inner = add(Int(1), Int(2)); // steps to Int(3)
//         let term = add(inner, Int(3));
//         let expected = add(Int(3), Int(3));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_add_right_not_value() {
//         // Right operand is not a value.
//         let inner = add(Int(2), Int(3)); // steps to Int(5)
//         let term = add(Int(1), inner);
//         let expected = add(Int(1), Int(5));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_add_both_values() {
//         let term = add(Int(3), Int(4));
//         let expected = Int(7);
//         assert_eq!(term.step(), expected);
//     }
// }

// mod sub {
//     use super::*;

//     #[test]
//     fn test_step_sub_left_not_value() {
//         let inner = sub(Int(5), Int(2)); // steps to Int(3)
//         let term = sub(inner, Int(4));
//         let expected = sub(Int(3), Int(4));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_sub_right_not_value() {
//         let inner = sub(Int(10), Int(3)); // steps to Int(7)
//         let term = sub(Int(20), inner);
//         let expected = sub(Int(20), Int(7));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_sub_both_values() {
//         let term = sub(Int(10), Int(4));
//         let expected = Int(6);
//         assert_eq!(term.step(), expected);
//     }
// }

// mod mul {
//     use super::*;

//     #[test]
//     fn test_step_mul_left_not_value() {
//         let inner = mul(Int(2), Int(3)); // steps to Int(6)
//         let term = mul(inner, Int(4));
//         let expected = mul(Int(6), Int(4));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_mul_right_not_value() {
//         let inner = mul(Int(3), Int(4)); // steps to Int(12)
//         let term = mul(Int(2), inner);
//         let expected = mul(Int(2), Int(12));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_mul_both_values() {
//         let term = mul(Int(3), Int(4));
//         let expected = Int(12);
//         assert_eq!(term.step(), expected);
//     }
// }

// mod eq {
//     use super::*;

//     #[test]
//     fn test_step_eq_left_not_value() {
//         let inner = add(Int(1), Int(2)); // steps to Int(3)
//         let term = eq(inner, Int(3));
//         let expected = eq(Int(3), Int(3));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_eq_right_not_value() {
//         let inner = add(Int(2), Int(3)); // steps to Int(5)
//         let term = eq(Int(5), inner);
//         let expected = eq(Int(5), Int(5));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_eq_both_values_equal_int() {
//         let term = eq(Int(7), Int(7));
//         // When both integer values are equal, eq returns True.
//         assert_eq!(term.step(), True);
//     }

//     #[test]
//     fn test_step_eq_both_values_not_equal_int() {
//         let term = eq(Int(7), Int(8));
//         assert_eq!(term.step(), False);
//     }
// }

// mod ne {
//     use super::*;

//     #[test]
//     fn test_step_ne_left_not_value() {
//         let inner = add(Int(1), Int(2));
//         let term = ne(inner, Int(3));
//         let expected = ne(Int(3), Int(3));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_ne_right_not_value() {
//         let inner = add(Int(2), Int(3));
//         let term = ne(Int(5), inner);
//         let expected = ne(Int(5), Int(5));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_ne_both_values_equal_int() {
//         let term = ne(Int(7), Int(7));
//         // When equal, ne returns False.
//         assert_eq!(term.step(), False);
//     }

//     #[test]
//     fn test_step_ne_both_values_not_equal_int() {
//         let term = ne(Int(7), Int(8));
//         assert_eq!(term.step(), True);
//     }
// }
// mod lt {
//     use super::*;

//     #[test]
//     fn test_step_lt_left_not_value() {
//         let inner = add(Int(1), Int(2)); // steps to Int(3)
//         let term = lt(inner, Int(5));
//         let expected = lt(Int(3), Int(5));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_lt_right_not_value() {
//         let inner = add(Int(2), Int(2)); // steps to Int(4)
//         let term = lt(Int(3), inner);
//         let expected = lt(Int(3), Int(4));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_lt_both_values_true() {
//         let term = lt(Int(3), Int(5));
//         assert_eq!(term.step(), True);
//     }

//     #[test]
//     fn test_step_lt_both_values_false() {
//         let term = lt(Int(5), Int(3));
//         assert_eq!(term.step(), False);
//     }
// }

// mod le {
//     use super::*;

//     #[test]
//     fn test_step_le_left_not_value() {
//         let inner = add(Int(1), Int(1)); // steps to Int(2)
//         let term = le(inner, Int(3));
//         let expected = le(Int(2), Int(3));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_le_right_not_value() {
//         let inner = add(Int(2), Int(1)); // steps to Int(3)
//         let term = le(Int(2), inner);
//         let expected = le(Int(2), Int(3));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_le_both_values_true() {
//         let term = le(Int(3), Int(3));
//         assert_eq!(term.step(), True);
//     }

//     #[test]
//     fn test_step_le_both_values_false() {
//         let term = le(Int(5), Int(3));
//         assert_eq!(term.step(), False);
//     }
// }

// mod gt {
//     use super::*;

//     #[test]
//     fn test_step_gt_left_not_value() {
//         let inner = add(Int(2), Int(2)); // steps to Int(4)
//         let term = gt(inner, Int(3));
//         let expected = gt(Int(4), Int(3));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_gt_right_not_value() {
//         let inner = add(Int(1), Int(1)); // steps to Int(2)
//         let term = gt(Int(5), inner);
//         let expected = gt(Int(5), Int(2));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_gt_both_values_true() {
//         let term = gt(Int(5), Int(3));
//         assert_eq!(term.step(), True);
//     }

//     #[test]
//     fn test_step_gt_both_values_false() {
//         let term = gt(Int(3), Int(5));
//         assert_eq!(term.step(), False);
//     }
// }

// mod ge {
//     use super::*;

//     #[test]
//     fn test_step_ge_left_not_value() {
//         let inner = add(Int(2), Int(2)); // steps to Int(4)
//         let term = ge(inner, Int(3));
//         let expected = ge(Int(4), Int(3));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_ge_right_not_value() {
//         let inner = add(Int(1), Int(2)); // steps to Int(3)
//         let term = ge(Int(5), inner);
//         let expected = ge(Int(5), Int(3));
//         assert_eq!(term.step(), expected);
//     }

//     #[test]
//     fn test_step_ge_both_values_true() {
//         let term = ge(Int(5), Int(5));
//         assert_eq!(term.step(), True);
//     }

//     #[test]
//     fn test_step_ge_both_values_false() {
//         let term = ge(Int(3), Int(5));
//         assert_eq!(term.step(), False);
//     }
// }

// mod pair {
//     use super::*;

//     // Test for attempting to step a pair value
//     #[test]
//     #[should_panic(expected = "attempted to step pair of values")]
//     fn test_step_pair() {
//         let t = pair(Int(1), Int(2));
//         let _ = t.step();
//     }

//     // Test for stepping fst when the argument is not a pair
//     #[test]
//     fn test_step_fst_not_value() {
//         let pair_term = pair(add(Int(1), Int(2)), Int(3)); // will step to pair(Int(3), Int(3))
//         let fst_term = fst(pair_term);
//         let expected = fst(pair(Int(3), Int(3)));
//         assert_eq!(fst_term.step(), expected);
//     }

//     // Test for stepping snd when the argument is not a pair
//     #[test]
//     fn test_step_snd_not_value() {
//         let pair_term = pair(Int(1), add(Int(2), Int(3))); // will step to pair(Int(1), Int(5))
//         let snd_term = snd(pair_term);
//         let expected = snd(pair(Int(1), Int(5)));
//         assert_eq!(snd_term.step(), expected);
//     }

//     // Test for stepping fst when the argument is a pair value
//     #[test]
//     fn test_step_fst_value() {
//         let t = fst(pair(Int(1), Int(2)));
//         let expected = Int(1);
//         assert_eq!(t.step(), expected);
//     }

//     // Test for stepping snd when the argument is a pair value
//     #[test]
//     fn test_step_snd_value() {
//         let t = snd(pair(Int(1), Int(2)));
//         let expected = Int(2);
//         assert_eq!(t.step(), expected);
//     }
// }

// mod list {
//     use super::*;

//     #[test]
//     fn test_step_lcase_app() {
//         assert_eq!(
//             lcase(
//                 app(abs("l", list(Integer), "l"), Nil(Integer)),
//                 Int(0),
//                 "h",
//                 "t",
//                 var("h")
//             )
//             .step(),
//             lcase(Nil(Integer), Int(0), "h", "t", var("h"))
//         );
//     }

//     #[test]
//     fn test_step_lcase_nil() {
//         assert_eq!(
//             lcase(Nil(Integer), Int(0), "h", "t", var("h")).step(),
//             Int(0)
//         );
//     }

//     #[test]
//     fn test_step_lcase_cons() {
//         assert_eq!(
//             lcase(cons(Int(1), Nil(Integer)), Int(0), "h", "t", var("h")).step(),
//             Int(1)
//         );

//         assert_eq!(
//             lcase(
//                 cons(Int(1), Nil(Integer)),
//                 cons(Int(2), Nil(Integer)),
//                 "h",
//                 "t",
//                 var("t")
//             )
//             .step(),
//             Nil(Integer)
//         );
//     }
// }

// mod sum {
//     use super::*;

//     #[test]
//     fn test_step_case_app() {
//         assert_eq!(
//             case(
//                 app(abs("s", sum(Boolean, Integer), "s"), inl(True, Integer)),
//                 "x",
//                 var("x"),
//                 "x",
//                 eq(var("x"), Int(0)),
//             )
//             .step(),
//             case(inl(True, Integer), "x", var("x"), "x", eq(var("x"), Int(0)))
//         );
//     }

//     #[test]
//     fn test_step_case_inl() {
//         assert_eq!(
//             case(inl(True, Integer), "x", var("x"), "x", eq(var("x"), Int(0))).step(),
//             True
//         );
//     }

//     #[test]
//     fn test_step_case_inr() {
//         assert_eq!(
//             case(
//                 inr(Int(1), Boolean),
//                 "x",
//                 var("x"),
//                 "x",
//                 eq(var("x"), Int(0)),
//             )
//             .step(),
//             eq(Int(1), Int(0))
//         );
//     }
// }
