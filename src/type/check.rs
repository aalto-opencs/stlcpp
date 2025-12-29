use std::collections::HashMap;

use super::TypeError::{self, *};
use crate::context::Context;
use crate::module::{Declaration, Module, ModuleTree};
use crate::term::parse::{
    Desugared, SpannedToken,
    Token::{self, *},
};
use crate::r#type::named_type::NamedType;
use crate::r#type::util::{arrow, list, sum};

impl<'a> SpannedToken<'a, Desugared> {
    /// Check whether this *desugared* spanned term token contains any type holes.
    ///
    /// This is intentionally implemented without converting into core [`Term`], because that
    /// conversion path does not have access to module-level type aliases and can fail for valid
    /// programs that use aliases in annotations.
    pub fn contains_holes_token(&self) -> bool {
        use Token::*;

        match &self.token {
            // Nodes that carry a type annotation
            Abs { ty, body, .. } => ty.contains_holes() || body.contains_holes_token(),
            Inl(t, ty) | Inr(t, ty) | TApp(t, ty) => {
                ty.contains_holes() || t.contains_holes_token()
            }
            Nil(ty) => ty.contains_holes(),
            Panic(_, ty, t) => ty.contains_holes() || t.contains_holes_token(),

            // Recursive term structure
            App(t1, t2)
            | Add(t1, t2)
            | Sub(t1, t2)
            | Mul(t1, t2)
            | Div(t1, t2)
            | Eq(t1, t2)
            | Ne(t1, t2)
            | Lt(t1, t2)
            | Le(t1, t2)
            | Gt(t1, t2)
            | Ge(t1, t2)
            | Pair(t1, t2)
            | Cons(t1, t2) => t1.contains_holes_token() || t2.contains_holes_token(),

            Let { val_t, body, .. } => val_t.contains_holes_token() || body.contains_holes_token(),

            Ite {
                cond,
                if_true,
                if_false,
            } => {
                cond.contains_holes_token()
                    || if_true.contains_holes_token()
                    || if_false.contains_holes_token()
            }

            Fst(t) | Snd(t) | Fix(t) | Trace(_, t) => t.contains_holes_token(),

            LCase {
                t, nil_t, cons_t, ..
            } => {
                t.contains_holes_token()
                    || nil_t.contains_holes_token()
                    || cons_t.contains_holes_token()
            }

            Case {
                t, inl_t, inr_t, ..
            } => {
                t.contains_holes_token()
                    || inl_t.contains_holes_token()
                    || inr_t.contains_holes_token()
            }

            TAbs { body, .. } => body.contains_holes_token(),

            Print(t) => t.contains_holes_token(),
            IOBind { func, t, .. } => func.contains_holes_token() || t.contains_holes_token(),
            IOPure(_, t) => t.contains_holes_token(),

            // Leaves / trivially hole-free nodes
            Var(_) | True | False | Int(_) | Char(_) | Trivial | ReadLine => false,

            // Desugared terms should not contain `Infix`, but treat it as "no holes" here.
            Infix { .. } | Prefix { .. } => false,
        }
    }

    /// Infer, i.e. construct, the type of this term.
    /// The constructed type is unique up to alpha-equivalence, as [`NamedType`]s use `usize`-indexed type variables.
    /// A returned type is always well formed in the context where the type was inferred.
    ///
    /// Takes in a "mutable" context and "static" type aliases to distinguish between the two.
    ///
    /// # Examples
    ///
    /// ```
    /// # use stlcpp::util::infer_type_src_string;
    /// let ty = infer_type_src_string(
    ///     &Default::default(),
    ///     "(fun X, fun x : X, x)",
    /// )
    /// .unwrap();
    /// assert_eq!(ty, "forall X, X -> X");
    /// ```
    ///
    /// ```rust
    /// # use stlcpp::util::infer_type_src_string;
    /// let ty = infer_type_src_string(
    ///     &Default::default(),
    ///     "(fun X, fun x : X, x) Integer 5",
    /// )
    /// .unwrap();
    /// assert_eq!(ty, "Int");
    /// ```
    ///
    /// # Notes
    ///
    /// In the [`Term::App`] branch of the match block (i.e. appl rule), two types need to be compared for "equality".
    ///
    /// In the `TApp` branch of the match block (i.e. appl2 rule), because ty2 may contain free variables (that are bound by the context), the substitution may lead to type variable capture.
    /// This is why the context disallows type variable shadowing.
    /// On term level, shadowing was okay, because we don't allow stepping the inner term which could lead to variable capturing substitution.
    #[allow(clippy::result_large_err)]
    pub fn infer_type(
        &'_ self,
        mut ctx: Context,
        aliases: &HashMap<String, NamedType>,
    ) -> Result<NamedType, TypeError<'a>> {
        match &self.token {
            // Definition 3.4.6
            Var(x) => ctx
                .get(x)
                .cloned()
                .ok_or_else(|| UndefinedVariable(x.clone())),

            // abst
            Abs { var, ty, body } => {
                let named_type: NamedType = ty
                    .token
                    .clone()
                    .to_named_type_ctx_with_aliases(ctx.tvars(), aliases)?;
                if !named_type.is_closed_ctx(&ctx.tvars()) {
                    return Err(TypeError::NotProperlyFormed(
                        var.clone(),
                        (ty.position, named_type),
                        ctx.into(),
                    ));
                }
                ctx.insert(var.clone(), named_type.clone());
                Ok(arrow(named_type, body.infer_type(ctx, aliases)?))
            }

            // appl
            App(term, term1) => {
                let ty1 = term.infer_type(ctx.clone(), aliases)?;
                if let NamedType::Arrow(dom, cod) = ty1 {
                    let ty2 = term1.infer_type(ctx.clone(), aliases)?;

                    if ty2 == *dom {
                        Ok(*cod)
                    } else if let Abs { ty, .. } = term.token.clone() {
                        Err(Mismatch(
                            (ty.position, *dom),
                            (term1.position, ty2),
                            ctx.into(),
                        ))
                    } else {
                        Err(Mismatch(
                            (term.position, *dom),
                            (term1.position, ty2),
                            ctx.into(),
                        ))
                    }
                } else {
                    Err(Mismatch(
                        (term1.position, arrow(NamedType::Hole, NamedType::Hole)),
                        (term.position, ty1),
                        ctx.into(),
                    ))
                }
            }

            Let { var, val_t, body } => {
                let ty = val_t.infer_type(ctx.clone(), aliases)?;
                ctx.insert(var.clone(), ty);
                body.infer_type(ctx, aliases)
            }
            True | False => Ok(NamedType::Boolean),
            Ite {
                cond,
                if_true,
                if_false,
            } => {
                let ty = cond.infer_type(ctx.clone(), aliases)?;
                if ty == NamedType::Boolean {
                    let ty1 = if_true.infer_type(ctx.clone(), aliases)?;
                    let ty2 = if_false.infer_type(ctx.clone(), aliases)?;
                    if ty2 != ty1 {
                        Err(Mismatch(
                            (if_true.position, ty1),
                            (if_false.position, ty2),
                            ctx.into(),
                        ))
                    } else {
                        Ok(ty1)
                    }
                } else {
                    Err(Mismatch(
                        (cond.position, NamedType::Boolean),
                        (cond.position, ty),
                        ctx.into(),
                    ))
                }
            }
            Int(_) => Ok(NamedType::Integer),
            Add(term, term1) | Sub(term, term1) | Mul(term, term1) | Div(term, term1) => {
                let ty1 = term.infer_type(ctx.clone(), aliases)?;
                let ty2 = term1.infer_type(ctx.clone(), aliases)?;
                if ty1 != NamedType::Integer {
                    return Err(Mismatch(
                        (self.position, NamedType::Integer),
                        (term.position, ty1),
                        ctx.into(),
                    ));
                }
                if ty2 != NamedType::Integer {
                    return Err(Mismatch(
                        (self.position, NamedType::Integer),
                        (term1.position, ty2),
                        ctx.into(),
                    ));
                }
                Ok(NamedType::Integer)
            }
            Eq(term, term1)
            | Ne(term, term1)
            | Lt(term, term1)
            | Le(term, term1)
            | Gt(term, term1)
            | Ge(term, term1) => {
                let ty1 = term.infer_type(ctx.clone(), aliases)?;
                let ty2 = term1.infer_type(ctx.clone(), aliases)?;
                use NamedType::*;
                match (ty1, ty2) {
                    (Integer, Integer) | (Character, Character) | (Boolean, Boolean) => Ok(Boolean),
                    (ty1, ty2) => Err(CantCompare(
                        (term.position, ty1),
                        (term1.position, ty2),
                        ctx.into(),
                    )),
                }
            }
            Char(_) => Ok(NamedType::Character),
            Trivial => Ok(NamedType::Unit),
            Pair(t1, t2) => {
                let ty1 = t1.infer_type(ctx.clone(), aliases)?;
                let ty2 = t2.infer_type(ctx.clone(), aliases)?;
                Ok(NamedType::Prod(ty1.into(), ty2.into()))
            }
            Fst(t) => {
                let ty = t.infer_type(ctx.clone(), aliases)?;

                use NamedType::*;
                if let Prod(ty1, _) = ty {
                    Ok(*ty1)
                } else {
                    Err(Mismatch(
                        (t.position, Prod(Box::new(Hole), Box::new(Hole))),
                        (t.position, ty),
                        ctx.into(),
                    ))
                }
            }
            Snd(t) => {
                let ty = t.infer_type(ctx.clone(), aliases)?;

                use NamedType::*;
                if let Prod(_, ty2) = ty {
                    Ok(*ty2)
                } else {
                    Err(Mismatch(
                        (t.position, Prod(Box::new(Hole), Box::new(Hole))),
                        (t.position, ty),
                        ctx.into(),
                    ))
                }
            }
            Nil(ty) => Ok(NamedType::List(Box::new(
                ty.token
                    .clone()
                    .to_named_type_ctx_with_aliases(ctx.tvars(), aliases)?,
            ))),
            Cons(head, tail) => {
                let tail_ty = tail.infer_type(ctx.clone(), aliases)?;
                let head_ty = head.infer_type(ctx.clone(), aliases)?;

                use NamedType::*;
                if let List(ty) = tail_ty {
                    if *ty != head_ty {
                        return Err(Mismatch(
                            (tail.position, *ty),
                            (head.position, head_ty),
                            ctx.into(),
                        ));
                    }
                    return Ok(List(ty));
                }

                Err(Mismatch(
                    (self.position, List(Box::new(head_ty))),
                    (tail.position, tail_ty),
                    ctx.into(),
                ))
            }
            LCase {
                t,
                nil_t,
                head_var,
                tail_var,
                cons_t,
            } => {
                let ty = t.infer_type(ctx.clone(), aliases)?;
                use NamedType::*;
                if let List(ty_elem) = ty {
                    let nil_ty = nil_t.infer_type(ctx.clone(), aliases)?;
                    let ty_elem2 = ty_elem.as_ref().clone();
                    // Induction hypothesis: ty and nil_ty are properly formed
                    ctx.insert(head_var.clone(), ty_elem2);
                    ctx.insert(tail_var.clone(), List(ty_elem));
                    let cons_ty = cons_t.infer_type(ctx.clone(), aliases)?;
                    if cons_ty != nil_ty {
                        Err(Mismatch(
                            (nil_t.position, nil_ty),
                            (cons_t.position, cons_ty),
                            ctx.into(),
                        ))
                    } else {
                        Ok(nil_ty)
                    }
                } else {
                    Err(Mismatch(
                        (self.position, list(NamedType::Hole)),
                        (t.position, ty),
                        ctx.into(),
                    ))
                }
            }
            Inl(term, ty_right) => {
                let ty_left = term.infer_type(ctx.clone(), aliases)?;
                Ok(NamedType::Sum(
                    ty_left.into(),
                    Box::new(
                        ty_right
                            .token
                            .clone()
                            .to_named_type_ctx_with_aliases(ctx.tvars(), aliases)?,
                    ),
                ))
            }
            Inr(term, ty_left) => {
                let ty_right = term.infer_type(ctx.clone(), aliases)?;
                Ok(NamedType::Sum(
                    Box::new(
                        ty_left
                            .token
                            .clone()
                            .to_named_type_ctx_with_aliases(ctx.tvars(), aliases)?,
                    ),
                    ty_right.into(),
                ))
            }
            Case {
                t,
                inl_var,
                inl_t,
                inr_var,
                inr_t,
            } => {
                let ty = t.infer_type(ctx.clone(), aliases)?;
                if let NamedType::Sum(ty_left, ty_right) = ty.clone() {
                    let mut ctx2 = ctx.clone();
                    // Induction hypothesis: ty, and therefore ty_left, is properly formed
                    ctx.insert(inl_var.clone(), *ty_left);
                    let left_ty = inl_t.infer_type(ctx.clone(), aliases)?;

                    // Induction hypothesis: ty, and therefore ty_right, is properly formed
                    ctx2.insert(inr_var.clone(), *ty_right);
                    let right_ty = inr_t.infer_type(ctx2, aliases)?;

                    if right_ty != left_ty {
                        Err(Mismatch(
                            (inl_t.position, left_ty),
                            (inr_t.position, right_ty),
                            ctx.into(),
                        ))
                    } else {
                        Ok(left_ty)
                    }
                } else {
                    Err(Mismatch(
                        (self.position, sum(NamedType::Hole, NamedType::Hole)),
                        (t.position, ty),
                        ctx.into(),
                    ))
                }
            }
            Fix(term) => {
                let ty = term.infer_type(ctx.clone(), aliases)?;
                use NamedType::*;
                if let Arrow(left, right) = ty {
                    if left != right {
                        return Err(Mismatch(
                            (term.position, *left),
                            (self.position, *right),
                            ctx.into(),
                        ));
                    }
                    Ok(*left)
                } else {
                    Err(Mismatch(
                        (self.position, arrow(NamedType::Hole, NamedType::Hole)),
                        (term.position, ty),
                        ctx.into(),
                    ))
                }
            }

            // appl2
            TApp(term, ty2) => {
                let ty1 = term.infer_type(ctx.clone(), aliases)?;
                if let NamedType::Abs(_tvar, tbody) = ty1 {
                    // ty2 must be a properly formed, i.e. contain no free type variables outside the current context
                    let named_type2: NamedType = ty2
                        .token
                        .clone()
                        .to_named_type_ctx_with_aliases(ctx.tvars(), aliases)?;
                    if !named_type2.is_closed_ctx(&ctx.tvars()) {
                        return Err(TAppRightNotClosed(ty2.clone()));
                    }
                    return Ok(tbody
                        .subst(0, named_type2.shift(1, 0))
                        .unshift(1, 0)
                        .expect("BUG: failed to unshift type"));
                } else {
                    Err(Mismatch(
                        (
                            self.position,
                            NamedType::Abs("_".to_string(), NamedType::Hole.into()),
                        ),
                        (term.position, ty1),
                        ctx.into(),
                    ))
                }
            }

            // abst2
            TAbs { var, body } => {
                ctx.insert_type(var.clone());
                Ok(NamedType::Abs(
                    var.to_string(),
                    Box::new(body.infer_type(ctx, aliases)?),
                ))
            }

            Panic(_, ty, t) => {
                // Ensure inner term type checks
                t.infer_type(ctx.clone(), aliases)?;
                Ok(ty
                    .token
                    .clone()
                    .to_named_type_ctx_with_aliases(ctx.tvars(), aliases)?)
            }

            Trace(_, term) => term.infer_type(ctx, aliases),
            Print(term) => {
                let ty = term.infer_type(ctx.clone(), aliases)?;
                if let NamedType::List(ty_inner) = &ty
                    && NamedType::Character == **ty_inner
                {
                    Ok(NamedType::IO(Box::new(NamedType::Unit)))
                } else {
                    Err(Mismatch(
                        (term.position, list(NamedType::Character)),
                        (term.position, ty),
                        ctx.into(),
                    ))
                }
            }
            ReadLine => Ok(NamedType::IO(Box::new(list(NamedType::Character)))),

            IOBind { dom, cod, func, t } => {
                let ty_dom: NamedType = dom
                    .token
                    .clone()
                    .to_named_type_ctx_with_aliases(ctx.tvars(), aliases)?;
                if !ty_dom.is_closed_ctx(&ctx.tvars()) {
                    // return Err(TAppRightNotClosed(dom.clone()));
                    todo!("not properly formed error");
                }
                let ty_cod: NamedType = cod
                    .token
                    .clone()
                    .to_named_type_ctx_with_aliases(ctx.tvars(), aliases)?;
                if !ty_cod.is_closed_ctx(&ctx.tvars()) {
                    // return Err(TAppRightNotClosed(cod.clone()));
                    todo!("not properly formed error");
                }
                let ty_func = func.infer_type(ctx.clone(), aliases)?;
                if let NamedType::Arrow(func_dom, func_cod) = ty_func {
                    if let NamedType::IO(func_cod1) = &*func_cod {
                        if *func_dom == ty_dom && **func_cod1 == ty_cod {
                            let ty = t.infer_type(ctx.clone(), aliases)?;
                            if let NamedType::IO(ty1) = ty.clone()
                                && *ty1 == ty_dom
                            {
                                Ok(*func_cod)
                            } else {
                                Err(Mismatch(
                                    (dom.position, NamedType::IO(ty_cod.into()).into()),
                                    (t.position, ty),
                                    ctx.into(),
                                ))
                            }
                        } else {
                            todo!("handle mismatch");
                        }
                    } else {
                        Err(Mismatch(
                            (func.position, NamedType::IO(NamedType::Hole.into())),
                            (func.position, *func_cod),
                            ctx.into(),
                        ))
                    }
                } else {
                    Err(Mismatch(
                        (
                            self.position,
                            NamedType::Arrow(ty_dom.into(), NamedType::IO(ty_cod.into()).into()),
                        ),
                        (func.position, ty_func),
                        ctx.into(),
                    ))
                }
            }
            IOPure(tty, tterm) => {
                let ty_term = tterm.infer_type(ctx.clone(), aliases)?;

                let ty: NamedType = tty
                    .token
                    .clone()
                    .to_named_type_ctx_with_aliases(ctx.tvars(), aliases)?;
                if ty == ty_term {
                    Ok(NamedType::IO(Box::new(ty_term)))
                } else {
                    Err(Mismatch(
                        (tty.position, ty),
                        (tterm.position, ty_term),
                        ctx.into(),
                    ))
                }
            }

            Infix { .. } | Prefix { .. } => unreachable!(
                "infer_type is implemented only for desugared terms which don't contain custom syntax"
            ),
        }
    }

    pub fn type_check(
        &'_ self,
        ctx: Context,
        aliases: &HashMap<String, NamedType>,
    ) -> Result<NamedType, TypeError<'a>> {
        let ty = self.infer_type(ctx, aliases)?;

        if self.contains_holes_token() {
            Err(TypeError::Holes)
        } else {
            Ok(ty)
        }
    }
}

impl super::parse::SpannedToken<'_> {
    pub fn contains_holes(&self) -> bool {
        self.token
            .clone()
            .to_named_type_ctx_with_aliases(vec![], &Default::default())
            .map(|c| c.contains_holes())
            .unwrap_or(false)
    }
}

impl<'a> Declaration<'a> {
    pub fn term_fix(&'_ self) -> SpannedToken<'a, Desugared> {
        SpannedToken::new((
            self.term.position,
            Token::Fix(Box::new(SpannedToken::new((
                self.ty.position,
                Token::Abs {
                    var: self.name.clone(),
                    ty: self.ty.clone().into(),
                    body: Box::new(self.term.clone()),
                },
            )))),
        ))
    }

    pub fn type_check(
        &'_ self,
        ctx: HashMap<String, NamedType>,
        aliases: &HashMap<String, NamedType>,
    ) -> Result<NamedType, TypeError<'a>> {
        let fixpoint = self.term_fix();
        let ty = fixpoint.type_check(ctx.into(), aliases)?;
        let declared = self
            .ty
            .token
            .clone()
            .to_named_type_ctx_with_aliases(vec![], aliases)?;

        if ty != declared {
            Err(TypeError::Mismatch(
                (self.ty.position, declared),
                (self.term.position, ty),
                vec![], // Top-level declaration can't be inside a forall
            ))
        } else {
            Ok(ty)
        }
    }
}

impl Module {
    /// Type-check a single module given an *already constructed* term environment.
    ///
    /// For a module tree, the initial environment should contain imported declarations (including
    /// the prelude), so names like `count` are in scope while checking this module.
    ///
    /// Note: module-level type aliases are type-checked/validated separately when building the
    /// alias environment (see `ModuleTree::type_alias_env`).
    pub fn type_check(
        &self,
        mut ctx: HashMap<String, NamedType>,
        aliases: &HashMap<String, NamedType>,
    ) -> Result<HashMap<String, NamedType>, TypeError<'static>> {
        for d in &self.declarations {
            if ctx.contains_key(&d.name) {
                panic!("duplicate declaration: {}", d.name)
            }
            let ty = d.type_check(ctx.clone(), aliases)?;
            ctx.insert(d.name.clone(), ty);
        }

        Ok(ctx)
    }
}

impl ModuleTree {
    /// Type-check an entire module tree (imports + local module) in a self-contained way.
    ///
    /// This method does not accept an external context or alias env because the result must
    /// depend only on this tree and its imports.
    pub fn type_check(&self) -> Result<HashMap<String, NamedType>, TypeError<'static>> {
        // 1) Type-check imports first and collect their term types into an environment.
        let mut imported_ctx: HashMap<String, NamedType> = Default::default();
        for m in &self.1 {
            let child_ctx = m.type_check()?;
            imported_ctx.extend(child_ctx);
        }

        // 2) Build the alias env after ensuring imports are valid (imports first, then local),
        // so type declarations are available where they should be.
        let aliases = self
            .type_alias_env()
            .map_err(|e| TypeError::UndefinedVariable(e.to_string()))?;

        // 3) Type-check the local module with imported declarations in scope.
        self.0.type_check(imported_ctx, &aliases)
    }
}
