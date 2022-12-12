use swc_core::{ecma::{
    ast::{Program, Stmt, IfStmt, BinExpr, op, Lit, MemberExpr, Ident},
    transforms::testing::test,
    visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
}, common::{DUMMY_SP, util::take::Take}};
use swc_core::plugin::{plugin_transform, proxies::TransformPluginProgramMetadata};


pub struct TransformVisitor;

impl VisitMut for TransformVisitor {
    // Implement necessary visit_mut_* methods for actual custom transform.
    // A comprehensive list of possible visitor methods can be found here:
    // https://rustdoc.swc.rs/swc_ecma_visit/trait.VisitMut.html
    fn visit_mut_stmt(&mut self, stmt: &mut Stmt) {
        stmt.visit_mut_children_with(self);
        
        let expr_stmt = match stmt.as_expr() {
            Some(es) => es,
            None => {return}
        };

        let call_expr = match expr_stmt.expr.as_call() {
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

        if sym != "warning" {return};

        let warning_call = stmt.take();
        let wrapped_warning = Stmt::from(IfStmt{
            span: DUMMY_SP,
            test: BinExpr{ 
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
            }.into(),
            cons: Box::new(warning_call),
            alt: None,
        });
        *stmt = wrapped_warning;
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
    r#"warning(true, "a", "b");"#,
    // Output codes after transformed with plugin
    r#"if ("production" !== process.env.NODE_ENV) warning(true, "a", "b");"#
);

test!(
    ignore,
    Default::default(),
    |_| as_folder(TransformVisitor),
    warning_import_substitution,
    // Input codes
    r#"import {warning as w} from "warning"; w(true, "a", "b");"#,
    // Output codes after transformed with plugin
    r#"import {warning as w} from "warning"; if ("production" !== process.env.NODE_ENV) w(true, "a", "b");"#
);