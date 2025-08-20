use crate::r#type::named_type::NamedType;

impl NamedType {
    // TODO
    // pub fn subst_all<'a, I: Iterator<Item = (usize, &'a Self)> + Clone>(mut self, map: I) -> Self {
    // todo!()
    // }

    /// Substitutes the variable at De Bruijn index `x` with the term `v`.
    pub fn subst(self, x: usize, v: Self) -> Self {
        match self {
            Self::Var(k) => {
                if k == x {
                    v
                } else {
                    Self::Var(k)
                }
            }

            // 2. Abstraction Case (The tricky part)
            // When we recurse under a binder:
            // - The target index `x` must be incremented (it is now one layer deeper).
            // - The replacement term `v` must be shifted, because it is moving inside
            //   a new scope, so its free variables need to be adjusted to point
            //   to the same outer binders.
            Self::Abs(name, body) => {
                let v_shifted = v.shift(1, 0);
                Self::Abs(name, Box::new(body.subst(x + 1, v_shifted)))
            }

            Self::Arrow(t1, t2) => {
                Self::Arrow(Box::new(t1.subst(x, v.clone())), Box::new(t2.subst(x, v)))
            }
            Self::Prod(t1, t2) => {
                Self::Prod(Box::new(t1.subst(x, v.clone())), Box::new(t2.subst(x, v)))
            }
            Self::Sum(t1, t2) => {
                Self::Sum(Box::new(t1.subst(x, v.clone())), Box::new(t2.subst(x, v)))
            }
            Self::List(t1) => Self::List(Box::new(t1.subst(x, v))),
            Self::IO(t1) => Self::IO(Box::new(t1.subst(x, v))),

            Self::Boolean | Self::Integer | Self::Character | Self::Unit | Self::Hole => self,
        }
    }

    /// Helper: Shifts (lifts) free De Bruijn indices in the term.
    ///
    /// `inc`: The amount to shift by (usually 1 when entering a binder).
    /// `cutoff`: The index threshold. Indices less than `cutoff` are bound
    ///           within `self` and should not be shifted.
    pub fn shift(self, inc: usize, cutoff: usize) -> Self {
        match self {
            Self::Var(k) => {
                if k < cutoff {
                    // Strictly local bound variable, don't change.
                    Self::Var(k)
                } else {
                    // Free variable (relative to cutoff), shift it.
                    Self::Var(k + inc)
                }
            }
            Self::Abs(name, body) => {
                // When entering a binder, the cutoff increases.
                Self::Abs(name, Box::new(body.shift(inc, cutoff + 1)))
            }
            Self::Arrow(t1, t2) => Self::Arrow(
                Box::new(t1.shift(inc, cutoff)),
                Box::new(t2.shift(inc, cutoff)),
            ),
            Self::Prod(t1, t2) => Self::Prod(
                Box::new(t1.shift(inc, cutoff)),
                Box::new(t2.shift(inc, cutoff)),
            ),
            Self::Sum(t1, t2) => Self::Sum(
                Box::new(t1.shift(inc, cutoff)),
                Box::new(t2.shift(inc, cutoff)),
            ),
            Self::List(t1) => Self::List(Box::new(t1.shift(inc, cutoff))),
            // Base types remain unchanged
            base => base,
        }
    }

    /// Decrements free De Bruijn indices by `amount`.
    ///
    /// This is used when a binder is removed (peeled off) from around the type.
    ///
    /// - `amount`: How many binders were removed (usually 1).
    /// - `cutoff`: The current binding depth. Indices `< cutoff` are bound
    ///             locally within `self` and are not modified.
    ///
    /// Returns `None` if `self` contains a variable that refers to one of the
    /// removed binders (an index `i` where `cutoff <= i < cutoff + amount`).
    pub fn unshift(self, amount: usize, cutoff: usize) -> Option<Self> {
        match self {
            // 1. Variable Logic
            Self::Var(k) => {
                if k < cutoff {
                    // Locally bound variable, ignore.
                    Some(Self::Var(k))
                } else if k >= cutoff + amount {
                    // Free variable defined outside the removed scope.
                    // We must decrement it to point to the correct slot
                    // in the smaller context.
                    Some(Self::Var(k - amount))
                } else {
                    // k is in the range [cutoff, cutoff + amount).
                    // This variable refers to the specific binder we are deleting.
                    // It is a "dangling pointer" error to unshift it.
                    None
                }
            }

            // 2. Abstraction: increment cutoff
            Self::Abs(name, body) => {
                let new_body = body.unshift(amount, cutoff + 1)?;
                Some(Self::Abs(name, Box::new(new_body)))
            }

            // 3. Recursive Types: propagate to children
            Self::Arrow(t1, t2) => Some(Self::Arrow(
                Box::new(t1.unshift(amount, cutoff)?),
                Box::new(t2.unshift(amount, cutoff)?),
            )),
            Self::Prod(t1, t2) => Some(Self::Prod(
                Box::new(t1.unshift(amount, cutoff)?),
                Box::new(t2.unshift(amount, cutoff)?),
            )),
            Self::Sum(t1, t2) => Some(Self::Sum(
                Box::new(t1.unshift(amount, cutoff)?),
                Box::new(t2.unshift(amount, cutoff)?),
            )),
            Self::List(t1) => Some(Self::List(Box::new(t1.unshift(amount, cutoff)?))),
            Self::IO(t1) => Some(Self::IO(Box::new(t1.unshift(amount, cutoff)?))),

            // 4. Base types
            Self::Boolean => Some(Self::Boolean),
            Self::Integer => Some(Self::Integer),
            Self::Character => Some(Self::Character),
            Self::Unit => Some(Self::Unit),
            Self::Hole => Some(Self::Hole),
        }
    }
}
