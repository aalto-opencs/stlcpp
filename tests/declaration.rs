// mod common;
// use common::util::*;

// #[test]
// fn test_to_term_fix_simple_integer() {
//     let decl = Declaration("x".to_string(), Integer, Int(5));
//     let body = var("y");
//     let expected = Let {
//         var: "x".to_string(),
//         val_t: Box::new(fix(abs("x", Integer, Int(5)))),
//         body: Box::new(var("y")),
//     };
//     assert_eq!(decl.to_term_fix(body), expected);
// }

// #[test]
// fn test_to_term_fix_with_function_type() {
//     let decl = Declaration(
//         "f".to_string(),
//         arrow(Integer, Integer),
//         abs("x", Integer, add(var("x"), Int(1))),
//     );
//     let body = app(var("f"), Int(2));
//     let expected = Let {
//         var: "f".to_string(),
//         val_t: Box::new(fix(abs(
//             "f",
//             arrow(Integer, Integer),
//             abs("x", Integer, add(var("x"), Int(1))),
//         ))),
//         body: Box::new(app(var("f"), Int(2))),
//     };
//     assert_eq!(decl.to_term_fix(body), expected);
// }

// #[test]
// fn test_to_term_fix_recursive_function() {
//     let decl = Declaration(
//         "fact".to_string(),
//         arrow(Integer, Integer),
//         abs(
//             "n",
//             Integer,
//             ite(
//                 eq(var("n"), Int(0)),
//                 Int(1),
//                 mul(var("n"), app(var("fact"), sub(var("n"), Int(1)))),
//             ),
//         ),
//     );
//     let body = app(var("fact"), Int(5));
//     let expected = Let {
//         var: "fact".to_string(),
//         val_t: Box::new(fix(abs(
//             "fact",
//             arrow(Integer, Integer),
//             abs(
//                 "n",
//                 Integer,
//                 ite(
//                     eq(var("n"), Int(0)),
//                     Int(1),
//                     mul(var("n"), app(var("fact"), sub(var("n"), Int(1)))),
//                 ),
//             ),
//         ))),
//         body: Box::new(app(var("fact"), Int(5))),
//     };
//     assert_eq!(decl.to_term_fix(body), expected);
// }

// #[test]
// fn test_to_term_fix_with_pair() {
//     let decl = Declaration("p".to_string(), prod(Integer, Boolean), pair(Int(10), True));
//     let body = fst(var("p"));
//     let expected = Let {
//         var: "p".to_string(),
//         val_t: Box::new(fix(abs("p", prod(Integer, Boolean), pair(Int(10), True)))),
//         body: Box::new(fst(var("p"))),
//     };
//     assert_eq!(decl.to_term_fix(body), expected);
// }
