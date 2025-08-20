use crate::module::Import;
use crate::term::Term;
use crate::r#type::named_type::NamedType;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Once;
use wasm_bindgen::prelude::*;

static PANIC_HOOK: Once = Once::new();

fn init_panic_hook() {
    PANIC_HOOK.call_once(|| {
        console_error_panic_hook::set_once();
    });
}

#[wasm_bindgen]
pub fn run(code: &str) -> String {
    init_panic_hook();

    // 1. Load Prelude
    let prelude_path = "std/prelude.stlc";
    let p_res =
        Import(PathBuf::from(prelude_path)).resolve(PathBuf::from("."), &Default::default());

    let p = match p_res {
        Ok(p) => p,
        Err(e) => return format!("Failed to load prelude: {}", e),
    };

    // 2. Parse User Code
    // We leak the code to get &'static str lifetime required by the parser
    let static_code = Box::leak(code.to_string().into_boxed_str());

    use crate::module::parse::parse_module;

    let mt = match parse_module(
        PathBuf::from("."),
        std::ffi::OsString::from("playground"),
        &p,
        static_code,
    ) {
        Ok(mt) => mt,
        Err(e) => return e.to_string(),
    };

    // Type-check the module tree (imports + local module) in a self-contained way.
    if let Err(e) = mt.type_check() {
        return e.to_string();
    };
    let env: HashMap<String, (Term, NamedType)> = mt.env().unwrap().into_iter().collect();

    // 6. Look for 'main' and execute
    if let Some((term, ty)) = env.get("main") {
        let env: HashMap<String, Term> = env
            .iter()
            .map(|(name, (t, _))| (name.clone(), t.clone()))
            .collect();

        let result = term.clone().multistep(&env);

        format!("{} :: {}", result, ty)
    } else {
        "Compiled successfully. Define 'main' to run.".to_string()
    }
}
