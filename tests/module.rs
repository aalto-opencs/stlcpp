// mod common;
// use common::util::*;

// #[test]
// fn test_new() {
//     let module = Module::new();
//     assert!(module.0.is_empty());
//     assert!(module.1.is_empty());
// }

// #[test]
// fn test_to_term_single_declaration() {
//     let module = Module(
//         vec![],
//         vec![Declaration("x".to_string(), Integer, Int(5))],
//         vec![],
//     );
//     let body = var("y");
//     let expected = letin("x", fix(abs("x", Integer, Int(5))), var("y"));
//     assert_eq!(module.to_term("/dummy/path", body).unwrap().0, expected);
// }

// #[test]
// fn test_to_term_multiple_declarations() {
//     let module = Module(
//         vec![],
//         vec![
//             Declaration("x".to_string(), Integer, Int(5)),
//             Declaration("y".to_string(), Integer, var("x")),
//         ],
//         vec![],
//     );
//     let body = var("z");
//     let expected = letin(
//         "x",
//         fix(abs("x", Integer, Int(5))),
//         letin("y", fix(abs("y", Integer, var("x"))), var("z")),
//     );
//     assert_eq!(module.to_term("/dummy/path", body).unwrap().0, expected);
// }

// #[test]
// fn test_to_term_import() {
//     let module = Module(
//         vec![Import("example.stlc".into())],
//         vec![Declaration("z".to_string(), Integer, var("a"))],
//         vec![],
//     );
//     let body = var("z");
//     let expected = letin(
//         "a",
//         fix(abs("a", Integer, Int(1))),
//         letin("z", fix(abs("z", Integer, var("a"))), body.clone()),
//     );

//     let tests_path = std::path::PathBuf::from(file!());
//     let mod_t = module
//         .to_term(tests_path.parent().unwrap(), body)
//         .unwrap()
//         .0;
//     assert_eq!(mod_t, expected);
// }

// // TODO test with syntax
