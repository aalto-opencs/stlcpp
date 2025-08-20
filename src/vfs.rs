use std::path::{Path, PathBuf};

static STD_DIR: include_dir::Dir<'_> = include_dir::include_dir!("$CARGO_MANIFEST_DIR/std");

pub fn get_file(path: &Path) -> Option<&'static str> {
    // Tries to fetch paths ending with `std/*` from the standard library
    let rest: PathBuf = path
        .components()
        .skip_while(|p| p.as_os_str() != "std")
        .skip(1)
        .collect();

    if rest.iter().count() > 0 {
        STD_DIR.get_file(rest).and_then(|f| f.contents_utf8())
    } else {
        None
    }
}
