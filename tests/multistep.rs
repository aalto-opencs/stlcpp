// mod common;
// use common::util::*;

// #[test]
// fn test_value() {
//     let inner_app = app(id2_(), id2_());
//     let term = abs("z", Boolean, inner_app);
//     // The abstraction is a value, so multistep should yield it unchanged.
//     assert_eq!(term.clone().multistep(), term);
// }

// #[test]
// fn test_church_pair() {
//     let t = app(
//         app(
//             abs(
//                 "x",
//                 Boolean,
//                 abs(
//                     "y",
//                     Boolean,
//                     abs("z", Boolean, app(app(var("z"), var("x")), var("y"))),
//                 ),
//             ),
//             id2("x"),
//         ),
//         id2("y"),
//     );

//     assert_eq!(
//         t.clone().multistep(),
//         abs("z", Boolean, app(app(var("z"), id2("x")), id2("y")))
//     );

//     let fst_t = app(abs("p", Boolean, app(var("p"), tru())), t.clone());
//     assert_eq!(fst_t.clone().multistep(), id2("x"));

//     let snd_t = app(abs("p", Boolean, app(var("p"), fals())), t.clone());
//     assert_eq!(snd_t.clone().multistep(), id2("y"));
// }

// #[test]
// fn test_let_nested() {
//     assert_eq!(
//         letin("x", id2("z"), letin("x", app(var("x"), id2("y")), var("x"))).multistep(),
//         id2("y")
//     );

//     assert_eq!(
//         letin("x", id2("z"), letin("y", app(var("x"), id2("y")), var("x"))).multistep(),
//         id2("z")
//     );

//     assert_eq!(
//         letin(
//             "x",
//             letin("x", id2("z"), app(var("x"), id2("y"))),
//             app(var("x"), id2("w"))
//         )
//         .multistep(),
//         id2("w")
//     );
// }

// #[test]
// fn test_let_not() {
//     assert_eq!(
//         letin(
//             "not",
//             abs("t", Boolean, ite(var("t"), False, True)),
//             app(var("not"), False)
//         )
//         .multistep(),
//         True
//     );
//     assert_eq!(
//         letin(
//             "not",
//             abs("t", Boolean, ite(var("t"), False, True)),
//             app(var("not"), True)
//         )
//         .multistep(),
//         False
//     );
// }

// #[test]
// fn test_complex_expression_with_comparisons() {
//     let condition = ite(le(add(Int(1), Int(2)), Int(3)), True, False);
//     let then_branch = app(
//         abs(
//             "f",
//             arrow(Integer, Integer),
//             sub(app(var("f"), Int(10)), mul(Int(2), Int(3))),
//         ),
//         abs("x", Integer, add(var("x"), mul(Int(1), Int(2)))),
//     );
//     let else_branch = app(abs("x", Integer, add(var("x"), Int(100))), Int(5));
//     let t = ite(condition, then_branch, else_branch);

//     assert_eq!(t.multistep(), Int(6).into())
// }

// #[test]
// fn test_complex_expression_with_pairs() {
//     let pair_condition = fst(pair(le(add(Int(1), Int(2)), Int(3)), Int(10)));
//     let complex_then_branch = fst(pair(
//         app(
//             abs(
//                 "f",
//                 arrow(Integer, Integer),
//                 sub(app(var("f"), Int(10)), mul(Int(2), Int(3))),
//             ),
//             abs("x", Integer, add(var("x"), mul(Int(1), Int(2)))),
//         ),
//         True,
//     ));
//     let complex_else_branch = snd(pair(
//         app(abs("x", Integer, add(var("x"), Int(100))), Int(5)),
//         Int(0),
//     ));

//     let complex_t = ite(pair_condition, complex_then_branch, complex_else_branch);

//     assert_eq!(complex_t.multistep(), Int(6));
// }

// #[test]
// fn test_fix() {
//     let fact_body = abs(
//         "x",
//         Integer,
//         ite(
//             eq(var("x"), Int(0)),
//             Int(1),
//             mul(var("x"), app(var("fact"), sub(var("x"), Int(1)))),
//         ),
//     );
//     let fact_term = Fix(Box::new(abs(
//         "fact",
//         arrow(Integer, Integer),
//         fact_body.clone(),
//     )));
//     let app_term = app(fact_term.clone(), Int(4));
//     let expected = Int(24); // 4! = 24
//     assert_eq!(app_term.multistep(), expected);
// }

// #[test]
// fn test_fix_factorial() {
//     let fact_body = abs(
//         "x",
//         Integer,
//         ite(
//             eq(var("x"), Int(0)),
//             Int(1),
//             mul(var("x"), app(var("fact"), sub(var("x"), Int(1)))),
//         ),
//     );
//     let fix_term = fix(abs("fact", arrow(Integer, Integer), fact_body.clone()));

//     // Applying factorial function to 4 should eventually yield 24 (4!)
//     let app_term = app(fix_term, Int(4));

//     // First step should substitute `fact` with the fixed-point combinator
//     let expected_step1 = app(
//         abs(
//             "x",
//             Integer,
//             ite(
//                 eq(var("x"), Int(0)),
//                 Int(1),
//                 mul(
//                     var("x"),
//                     app(
//                         fix(abs("fact", arrow(Integer, Integer), fact_body.clone())),
//                         sub(var("x"), Int(1)),
//                     ),
//                 ),
//             ),
//         ),
//         Int(4),
//     );

//     assert_eq!(app_term.clone().step(), expected_step1);

//     // Complete the multi-step reduction
//     let expected_multi_step = Int(24);
//     assert_eq!(app_term.multistep(), expected_multi_step);
// }

// #[test]
// fn test_fix_fibonacci() {
//     let fib_body = abs(
//         "n",
//         Integer,
//         ite(
//             le(var("n"), Int(1)),
//             var("n"),
//             add(
//                 app(var("fib"), sub(var("n"), Int(1))),
//                 app(var("fib"), sub(var("n"), Int(2))),
//             ),
//         ),
//     );
//     let fix_term = fix(abs("fib", arrow(Integer, Integer), fib_body.clone()));

//     // Applying Fibonacci function to 4 should eventually yield 3 (fib(4))
//     let app_term = app(fix_term, Int(4));

//     // First step should substitute `fib` with the fixed-point combinator
//     let expected_step1 = app(
//         abs(
//             "n",
//             Integer,
//             ite(
//                 le(var("n"), Int(1)),
//                 var("n"),
//                 add(
//                     app(
//                         fix(abs("fib", arrow(Integer, Integer), fib_body.clone())),
//                         sub(var("n"), Int(1)),
//                     ),
//                     app(
//                         fix(abs("fib", arrow(Integer, Integer), fib_body.clone())),
//                         sub(var("n"), Int(2)),
//                     ),
//                 ),
//             ),
//         ),
//         Int(4),
//     );

//     assert_eq!(app_term.clone().step(), expected_step1);

//     // Complete the multi-step reduction
//     let expected_multi_step = Int(3);
//     assert_eq!(app_term.multistep(), expected_multi_step);
// }

// #[test]
// fn test_simple_recursive_function() {
//     // Corrected simple recursive function that decrements a number until 0
//     let simple_recursive_body = abs(
//         "n",
//         Integer,
//         ite(
//             eq(var("n"), Int(0)),
//             Int(0),                                    // Base case
//             app(var("simple"), sub(var("n"), Int(1))), // Recursive case
//         ),
//     );
//     let fix_term = fix(abs(
//         "simple",
//         arrow(Integer, Integer),
//         simple_recursive_body.clone(),
//     ));

//     // Applying our simple recursive function to 3 should eventually yield 0
//     let app_term = app(fix_term, Int(3));

//     // First step should substitute `simple` with the fixed-point combinator
//     let expected_step1 = app(
//         abs(
//             "n",
//             Integer,
//             ite(
//                 eq(var("n"), Int(0)),
//                 Int(0),
//                 app(
//                     fix(abs(
//                         "simple",
//                         arrow(Integer, Integer),
//                         simple_recursive_body.clone(),
//                     )),
//                     sub(var("n"), Int(1)),
//                 ),
//             ),
//         ),
//         Int(3),
//     );

//     assert_eq!(app_term.clone().step(), expected_step1);

//     // Complete the multi-step reduction
//     assert_eq!(app_term.multistep(), Int(0));
// }

// #[test]
// fn test_nested_fix() {
//     let inner = fix(abs(
//         "inner",
//         arrow(Integer, Integer),
//         abs(
//             "x",
//             Integer,
//             ite(
//                 lt(var("x"), Int(5)),
//                 Int(1),
//                 add(
//                     app(var("inner"), sub(var("x"), Int(1))),
//                     app(var("outer"), sub(var("x"), Int(1))),
//                 ),
//             ),
//         ),
//     ));

//     let outer = fix(abs("outer", arrow(Integer, Integer), inner.clone()));

//     let app_term = app(outer.clone(), Int(10));

//     let expected_step1 = app(
//         fix(abs(
//             "inner",
//             arrow(Integer, Integer),
//             abs(
//                 "x",
//                 Integer,
//                 ite(
//                     lt(var("x"), Int(5)),
//                     Int(1),
//                     add(
//                         app(var("inner"), sub(var("x"), Int(1))),
//                         app(outer.clone(), sub(var("x"), Int(1))),
//                     ),
//                 ),
//             ),
//         )),
//         Int(10),
//     );

//     assert_eq!(app_term.clone().step(), expected_step1);

//     let expected_step2 = app(
//         abs(
//             "x",
//             Integer,
//             ite(
//                 lt(var("x"), Int(5)),
//                 Int(1),
//                 add(
//                     // The substitution happens inside the previously substituted inner
//                     app(inner.subst("outer", outer.clone()), sub(var("x"), Int(1))),
//                     app(outer, sub(var("x"), Int(1))),
//                 ),
//             ),
//         ),
//         Int(10),
//     );

//     assert_eq!(expected_step1.step(), expected_step2);

//     assert_eq!(app_term.multistep(), Int(64));
// }
