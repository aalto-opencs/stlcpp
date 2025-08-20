mod common;
use common::util::*;

use num_bigint::BigInt;

#[test]
fn test_is_value() {
    assert!(id2("x").is_value());
    assert!(tru().is_value());

    assert!(!app(id2("y"), id2("z")).is_value());
    assert!(!app(tru(), fals()).is_value());

    assert!(!letin("x", id2_(), id2_()).is_value());

    assert!(True.is_value());
    assert!(False.is_value());
    assert!(!ite(id2_(), id2_(), id2_()).is_value());

    assert!(Int(BigInt::from(1)).is_value());
    assert!(!add(Int(BigInt::from(1)), Int(BigInt::from(1))).is_value());
    assert!(!sub(Int(BigInt::from(1)), Int(BigInt::from(1))).is_value());
    assert!(!mul(Int(BigInt::from(1)), Int(BigInt::from(1))).is_value());
    assert!(!eq(Int(BigInt::from(1)), Int(BigInt::from(1))).is_value());
    assert!(!ne(Int(BigInt::from(1)), Int(BigInt::from(1))).is_value());
    assert!(!le(Int(BigInt::from(1)), Int(BigInt::from(1))).is_value());
    assert!(!lt(Int(BigInt::from(1)), Int(BigInt::from(1))).is_value());
    assert!(!ge(Int(BigInt::from(1)), Int(BigInt::from(1))).is_value());
    assert!(!gt(Int(BigInt::from(1)), Int(BigInt::from(1))).is_value());

    assert!(pair(tru(), Int(BigInt::from(1))).is_value());
    assert!(!pair(app(tru(), fals()), Int(BigInt::from(1))).is_value());
    assert!(!pair(Int(BigInt::from(1)), app(tru(), fals())).is_value());
    assert!(!fst(pair(tru(), Int(BigInt::from(1)))).is_value());
    assert!(!snd(pair(tru(), Int(BigInt::from(1)))).is_value());

    assert!(Nil(Integer).is_value());
    assert!(cons(Int(BigInt::from(1)), Nil(Integer)).is_value());
    assert!(
        cons(
            Int(BigInt::from(1)),
            cons(Int(BigInt::from(2)), Nil(Integer))
        )
        .is_value()
    );
    assert!(!cons(Int(BigInt::from(1)), app(tru(), fals())).is_value());
    assert!(!cons(app(tru(), fals()), Nil(Integer)).is_value());
    assert!(
        !lcase(
            cons(Int(BigInt::from(1)), Nil(Integer)),
            Int(BigInt::from(0)),
            "h",
            "t",
            var("h")
        )
        .is_value()
    );

    assert!(inl(Int(BigInt::from(1)), Boolean).is_value());
    assert!(inr(Int(BigInt::from(2)), Boolean).is_value());
    assert!(!inl(app(tru(), fals()), Boolean).is_value());
    assert!(!inr(app(tru(), fals()), Boolean).is_value());
    assert!(
        !case(
            inl(Int(BigInt::from(1)), Boolean),
            "x",
            var("x"),
            "y",
            var("y"),
        )
        .is_value()
    );

    assert!(!fix(id2_()).is_value());
    // Whether variable is a value is not tested, as it's not relevant
}
