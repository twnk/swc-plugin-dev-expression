/* This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.

 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
 * for more details.

 * You should have received a copy of the GNU General Public License along with
 * this program. If not, see <https://www.gnu.org/licenses/>.
*/

use swc_core::plugin::{plugin_transform, proxies::TransformPluginProgramMetadata};
use swc_core::{
  common::{util::take::Take, DUMMY_SP},
  ecma::{
    ast::{
      op, BinExpr, BlockStmt, CallExpr, CondExpr, Expr, ExprOrSpread, ExprStmt, Ident,
      IfStmt, Lit, MemberExpr, ParenExpr, Program, Stmt, UnaryExpr, UnaryOp,
    },
    transforms::testing::test,
    visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
  },
};

pub struct TransformVisitor;

// Returns the __DEV__ transformation `"production" !== process.env.NODE_ENV`
fn dev_expression() -> Box<Expr> {
  BinExpr {
    span: DUMMY_SP,
    op: op!("!=="),
    left: Lit::Str("production".into()).into(),
    right: MemberExpr {
      span: DUMMY_SP,
      obj: MemberExpr {
        span: DUMMY_SP,
        obj: Ident::new("process".into(), DUMMY_SP).into(),
        prop: Ident::new("env".into(), DUMMY_SP).into(),
      }
      .into(),
      prop: Ident::new("NODE_ENV".into(), DUMMY_SP).into(),
    }
    .into(),
  }
  .into()
}

// Transforms
// `warning(condition, "argument", argument);"`
// into
// `if ("production" !== process.env.NODE_ENV) {
//    warning(condition, "argument", argument);
//  }`
fn wrap_in_if_not_prod(stmt: Stmt, alt: Option<Stmt>) -> Stmt {
  let boxed_alt = alt.map(|s| {
    Box::new(Stmt::from(BlockStmt {
      span: DUMMY_SP,
      stmts: vec![s],
    }))
  });
  Stmt::from(IfStmt {
    span: DUMMY_SP,
    test: dev_expression(),
    cons: Box::new(Stmt::from(BlockStmt {
      span: DUMMY_SP,
      stmts: vec![stmt],
    })),
    alt: boxed_alt,
  })
}

// Transforms
//  `invariant(condition, argument, argument);`
// into
//  `if (!condition) {
//    if ("production" !== process.env.NODE_ENV) {
//      invariant(false, argument, argument);
//    } else {
//      invariant(false);
//    }
//  }`
fn wrap_invariant(invariant: &mut CallExpr) -> Stmt {
  if invariant.args.is_empty() {
    return Stmt::dummy();
  }
  if invariant.args[0].spread.is_some() {
    return Stmt::dummy();
  }
  let condition = match invariant.args.len() {
    0 => return Stmt::dummy(),
    _ => match invariant.args[0].spread {
      Some(_) => return Stmt::dummy(),
      None => invariant.args[0].expr.take(),
    },
  };

  let not_condition = UnaryExpr {
    span: DUMMY_SP,
    op: op!("!"),
    arg: condition,
  };

  let false_expr = ExprOrSpread {
    spread: None,
    expr: false.into(),
  };

  let invariant_no_args = ExprStmt {
    span: DUMMY_SP,
    expr: CallExpr {
      span: DUMMY_SP,
      callee: invariant.callee.clone(),
      args: vec![false_expr.clone()],
      type_args: None,
    }
    .into(),
  }
  .into();

  let mut args = invariant.args.take();
  args[0] = false_expr;
  let invariant_with_args = ExprStmt {
    span: DUMMY_SP,
    expr: CallExpr {
      span: DUMMY_SP,
      callee: invariant.callee.take(),
      args,
      type_args: None,
    }
    .into(),
  }
  .into();

  let inner_if = wrap_in_if_not_prod(invariant_with_args, Some(invariant_no_args));

  Stmt::from(IfStmt {
    span: DUMMY_SP,
    test: not_condition.into(),
    cons: Box::new(Stmt::from(BlockStmt {
      span: DUMMY_SP,
      stmts: vec![inner_if],
    })),
    alt: None,
  })
}

impl VisitMut for TransformVisitor {
  // Visits statements, matching function calls to invariant or warning
  fn visit_mut_stmt(&mut self, stmt: &mut Stmt) {
    stmt.visit_mut_children_with(self);

    let expr_stmt = match stmt.as_mut_expr() {
      Some(es) => es,
      None => return,
    };

    let call_expr = match expr_stmt.expr.as_mut_call() {
      Some(ce) => ce,
      None => return,
    };

    let callee_expr = match call_expr.callee.as_expr() {
      Some(be) => be,
      None => return,
    };

    let ident = match callee_expr.as_ident() {
      Some(i) => i,
      None => return,
    };

    let sym = &*ident.sym;

    match sym {
      "warning" => {
        *stmt = wrap_in_if_not_prod(stmt.take(), None);
      }
      "invariant" => {
        *stmt = wrap_invariant(call_expr);
      }
      _ => {},
    }
  }

  // Visits !unary expressions, looking for __DEV__
  fn visit_mut_unary_expr(&mut self, expr: &mut UnaryExpr) {
    expr.visit_mut_children_with(self);

    match expr.op {
      UnaryOp::Bang => {}
      _ => return,
    }

    let ident = match expr.arg.as_ident() {
      Some(i) => i,
      None => return,
    };

    let sym = &*ident.sym;

    if let "__DEV__" = sym {
      expr.arg = ParenExpr {
        span: DUMMY_SP,
        expr: dev_expression(),
      }
      .into();
    };
  }

  // Visits binary (`&&`,`||`) expressions, looking for __DEV__
  fn visit_mut_bin_expr(&mut self, expr: &mut BinExpr) {
    expr.visit_mut_children_with(self);

    match expr.op {
      swc_core::ecma::ast::BinaryOp::LogicalOr => {}
      swc_core::ecma::ast::BinaryOp::LogicalAnd => {}
      _ => return,
    }

    match expr.left.as_ident() {
      Some(ident) => {
        let sym = &*ident.sym;

        if let "__DEV__" = sym {
          *expr.left = ParenExpr {
            span: DUMMY_SP,
            expr: dev_expression(),
          }
          .into();
        };
      }
      None => {}
    };

    match expr.right.as_ident() {
      Some(ident) => {
        let sym = &*ident.sym;

        if let "__DEV__" = sym {
          *expr.right = ParenExpr {
            span: DUMMY_SP,
            expr: dev_expression(),
          }
          .into();
        };
      }
      None => {}
    };
  }

  // Visits conditional expressions `cond ? true : false`, looking for __DEV__
  fn visit_mut_cond_expr(&mut self, expr: &mut CondExpr) {
    expr.visit_mut_children_with(self);

    let ident = match expr.test.as_ident() {
      Some(i) => i,
      None => return,
    };

    let sym = &*ident.sym;

    if let "__DEV__" = sym {
      expr.test = dev_expression();
    };
  }

  // Visits if statements, looking for __DEV__
  fn visit_mut_if_stmt(&mut self, stmt: &mut IfStmt) {
    stmt.visit_mut_children_with(self);

    let ident = match stmt.test.as_ident() {
      Some(i) => i,
      None => return,
    };

    let sym = &*ident.sym;

    if let "__DEV__" = sym {
      stmt.test = dev_expression();
    };
  }
}

#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
  program.fold_with(&mut as_folder(TransformVisitor))
}

test!(
  Default::default(),
  |_| as_folder(TransformVisitor),
  warning_substitution,
  // Input codes
  r#"warning(condition, "argument", argument);"#,
  // Output codes after transformed with plugin
  r#"if ("production" !== process.env.NODE_ENV) {warning(condition, "argument", argument);}"#
);

test!(
  Default::default(),
  |_| as_folder(TransformVisitor),
  invariant_substitution,
  // Input codes
  r#"invariant(condition, argument, argument);"#,
  // Output codes after transformed with plugin
  r#"if (!condition) {
    if ("production" !== process.env.NODE_ENV) {
      invariant(false, argument, argument);
    } else {
      invariant(false);
    }
    }"#
);

test!(
  Default::default(),
  |_| as_folder(TransformVisitor),
  cond_dev_expr,
  // Input codes
  r#"const x = __DEV__ ? 'a' : 'b';"#,
  // Output codes after transformed with plugin
  r#"const x = "production" !== process.env.NODE_ENV ? 'a' : 'b';"#
);

test!(
  Default::default(),
  |_| as_folder(TransformVisitor),
  if_dev_expr,
  // Input codes
  r#"if (__DEV__) {toot()}"#,
  // Output codes after transformed with plugin
  r#"if ("production" !== process.env.NODE_ENV) {toot();}"#
);

test!(
  Default::default(),
  |_| as_folder(TransformVisitor),
  unary_dev_expr_not,
  // Input codes
  r#"if (!__DEV__) {dont_toot()}"#,
  // Output codes after transformed with plugin
  r#"if (!("production" !== process.env.NODE_ENV)) {dont_toot();}"#
);

test!(
  Default::default(),
  |_| as_folder(TransformVisitor),
  binary_dev_expr_and,
  // Input codes
  r#"__DEV__ && true"#,
  // Output codes after transformed with plugin
  r#""production" !== process.env.NODE_ENV && true;"#
);

test!(
  Default::default(),
  |_| as_folder(TransformVisitor),
  binary_dev_expr_or,
  // Input codes
  r#"__DEV__ || false"#,
  // Output codes after transformed with plugin
  r#""production" !== process.env.NODE_ENV || false;"#
);
