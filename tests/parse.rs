// mod common;
// use common::util::*;
// use nom::IResult;

// fn parse_term(input: &str) -> IResult<&str, Term> {
//     stlcpp::term::parse::parse_term(&[], input)
// }

// mod parse_char {
//     use super::*;

//     #[test]
//     fn simple_ascii() {
//         assert_eq!(parse_term("'a'"), Ok(("", Char('a'))));
//         assert!(parse_term("'ab'").is_err());
//     }

//     #[test]
//     fn escape_sequences() {
//         assert_eq!(parse_term(r#"'\u{1F602}'"#), Ok(("", Char('😂'))));
//     }

//     #[test]
//     fn complex_unicode() {
//         assert_eq!(parse_term("'😂'"), Ok(("", Char('😂'))));
//         assert!(parse_term("'🏴‍☠️'").is_err());
//     }
// }
// mod parse_string {
//     use super::*;

//     #[test]
//         assert_eq!(
//             parse_term(r#""abc""#),
//             Ok((
//                 "",
//                 cons(Char('a'), cons(Char('b'), cons(Char('c'), nil(Character))))
//             ))
//         );
//     }

//     #[test]
//     fn escape_sequences() {
//         assert_eq!(
//             parse_term(r#""\n\r\t\"\\""#),
//             Ok((
//                 "",
//                 cons(
//                     Char('\n'),
//                     cons(
//                         Char('\r'),
//                         cons(
//                             Char('\t'),
//                             cons(Char('"'), cons(Char('\\'), nil(Character)))
//                         )
//                     )
//                 )
//             ))
//         );
//         assert!(parse_term(r#""\n\r\t\"\\"#).is_err()); // The string is missing the final "
//         assert_eq!(
//             parse_term(r#""\u{200d}☠\u{fe0f}""#),
//             Ok((
//                 "",
//                 cons(
//                     Char('\u{200d}'),
//                     cons(Char('☠'), cons(Char('\u{fe0f}'), nil(Character)))
//                 )
//             ))
//         );
//     }

//     #[test]
//     fn complex_unicode() {
//         assert_eq!(
//             parse_term(r#""🏴‍☠️""#),
//             Ok((
//                 "",
//                 cons(
//                     Char('🏴'),
//                     cons(
//                         Char('\u{200d}'),
//                         cons(Char('☠'), cons(Char('\u{fe0f}'), nil(Character)))
//                     )
//                 )
//             ))
//         );
//         assert_eq!(
//             parse_term(r#""🇫🇮\u{200d}🏴‍☠️""#),
//             Ok((
//                 "",
//                 cons(
//                     Char('🇫'),
//                     cons(
//                         Char('🇮'),
//                         cons(
//                             Char('\u{200d}'),
//                             cons(
//                                 Char('🏴'),
//                                 cons(
//                                     Char('\u{200d}'),
//                                     cons(Char('☠'), cons(Char('\u{fe0f}'), nil(Character)))
//                                 )
//                             )
//                         )
//                     )
//                 )
//             ))
//         );
//     }
// }
