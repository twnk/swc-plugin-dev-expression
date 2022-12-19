use swc_core::{ecma::{
    ast::{Program, Stmt, IfStmt, Expr, BinExpr, op, Lit, MemberExpr, Ident, UnaryExpr, CallExpr, ExprOrSpread, ExprStmt, BlockStmt},
    transforms::testing::test,
    visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
}, common::{DUMMY_SP, util::take::Take}};
use swc_core::plugin::{plugin_transform, proxies::TransformPluginProgramMetadata};


pub struct TransformVisitor;

fn dev_expression() -> Box<Expr> {
    BinExpr{ 
        span: DUMMY_SP, 
        op: op!("!=="), 
        left: Lit::Str("production".into()).into(), 
        right: MemberExpr{ 
            span: DUMMY_SP, 
            obj: MemberExpr{
                span: DUMMY_SP,
                obj: Ident::new("process".into(), DUMMY_SP).into(),
                prop: Ident::new("env".into(), DUMMY_SP).into(),
            }.into(), 
            prop: Ident::new("NODE_ENV".into(), DUMMY_SP).into() 
        }.into() 
    }.into()
}

// Transforms
// `warning(condition, "argument", argument);"`
// into
// `if ("production" !== process.env.NODE_ENV) {
//      warning(condition, "argument", argument);
//  }`
fn wrap_in_if_not_prod(stmt: Stmt, alt: Option<Stmt>) -> Stmt {
    let boxed_alt = alt.map(|s| Box::new(Stmt::from(BlockStmt{ span: DUMMY_SP, stmts: vec![s] })));
    Stmt::from(IfStmt{
        span: DUMMY_SP,
        test: dev_expression(),
        cons: Box::new(Stmt::from(BlockStmt{ span: DUMMY_SP, stmts: vec![stmt] })),
        alt: boxed_alt,
    })
}


// Transforms 
//  `invariant(condition, argument, argument);`
// into
//  `if (!condition) {
//      if ("production" !== process.env.NODE_ENV) {
//          invariant(false, argument, argument);
//      } else {
//          invariant(false);
//      }
//  }`
fn wrap_invariant(invariant: &mut CallExpr) -> Stmt {
    if invariant.args.is_empty() {return Stmt::dummy()}
    if invariant.args[0].spread.is_some() {return Stmt::dummy()}
    let condition = match invariant.args.len() {
        0 => {return Stmt::dummy()},
        _ => {match invariant.args[0].spread {
            Some(_) => {return Stmt::dummy()}
            None => {invariant.args[0].expr.take()}
        }}
    };

    let not_condition= UnaryExpr{
        span: DUMMY_SP, 
        op: op!("!"), 
        arg: condition
    };

    let false_expr = ExprOrSpread{
        spread: None,
        expr: false.into()
    };

    let invariant_no_args = ExprStmt{
        span: DUMMY_SP,
        expr: CallExpr{
            span: DUMMY_SP,
            callee: invariant.callee.clone(),
            args: vec![false_expr.clone()],
            type_args: None,
        }.into()
    }.into();

    let mut args = invariant.args.take();
    args[0] = false_expr;
    let invariant_with_args = ExprStmt{
        span: DUMMY_SP,
        expr: CallExpr{
            span: DUMMY_SP,
            callee: invariant.callee.take(),
            args,
            type_args: None,
        }.into()
    }.into();

    let inner_if = wrap_in_if_not_prod(invariant_with_args, Some(invariant_no_args));

    Stmt::from(IfStmt{
        span: DUMMY_SP,
        test: not_condition.into(),
        cons: Box::new(Stmt::from(BlockStmt{ span: DUMMY_SP, stmts: vec![inner_if] })),
        alt: None,
    })
}


impl VisitMut for TransformVisitor {
    // Implement necessary visit_mut_* methods for actual custom transform.
    // A comprehensive list of possible visitor methods can be found here:
    // https://rustdoc.swc.rs/swc_ecma_visit/trait.VisitMut.html
    fn visit_mut_stmt(&mut self, stmt: &mut Stmt) {
        stmt.visit_mut_children_with(self);
        
        let expr_stmt = match stmt.as_mut_expr() {
            Some(es) => es,
            None => {return}
        };

        let call_expr = match expr_stmt.expr.as_mut_call() {
            Some(ce) => ce,
            None => {return}
        };

        let callee_expr = match call_expr.callee.as_expr() {
            Some(be) => be,
            None => {return}
        };

        let ident = match callee_expr.as_ident() {
            Some(i) => i,
            None => {return}
        };

        let sym = &*ident.sym;

        match sym {
            "warning" => {
                *stmt = wrap_in_if_not_prod(stmt.take(), None);
            },
            "invariant" => {
                *stmt = wrap_invariant(call_expr);},
            _ => {}
        }
    }
}

/// An example plugin function with macro support.
/// `plugin_transform` macro interop pointers into deserialized structs, as well
/// as returning ptr back to host.
///
/// It is possible to opt out from macro by writing transform fn manually
/// if plugin need to handle low-level ptr directly via
/// `__transform_plugin_process_impl(
///     ast_ptr: *const u8, ast_ptr_len: i32,
///     unresolved_mark: u32, should_enable_comments_proxy: i32) ->
///     i32 /*  0 for success, fail otherwise.
///             Note this is only for internal pointer interop result,
///             not actual transform result */`
///
/// This requires manual handling of serialization / deserialization from ptrs.
/// Refer swc_plugin_macro to see how does it work internally.
#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    program.fold_with(&mut as_folder(TransformVisitor))
}

// An example to test plugin transform.
// Recommended strategy to test plugin's transform is verify
// the Visitor's behavior, instead of trying to run `process_transform` with mocks
// unless explicitly required to do so.
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