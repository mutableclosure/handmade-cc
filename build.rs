use serde_json::Value;
use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use walkdir::WalkDir;

const EXPECTED_RESULTS: &str = "src/test/data/expected-results.json";
const TESTS_DIR: &str = "src/test/data/tests";
const EXTENSION: &str = "c";
const RETURN_CODE: &str = "return_code";
const OUT_DIR: &str = "OUT_DIR";
const GENERATED_TESTS: &str = "generated_tests.rs";

fn main() {
    let expected_results = fs::read_to_string(EXPECTED_RESULTS).unwrap();
    let expected_results: Value = serde_json::from_str(&expected_results).unwrap();
    let source_files = read_source_files();

    let out_dir = PathBuf::from(env::var_os(OUT_DIR).unwrap());
    let output_path = out_dir.join(GENERATED_TESTS);
    let mut output = File::create(&output_path).unwrap();

    for source_file in source_files {
        let path = PathBuf::from(source_file);
        let path = path
            .strip_prefix(TESTS_DIR)
            .unwrap()
            .to_string_lossy()
            .into_owned();
        let expected_return_code = read_return_code(&expected_results, &path);
        let function_name = to_function_name(&path);
        write_test_case(&mut output, &function_name, &path, expected_return_code);
    }

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/test/data");
}

fn read_source_files() -> Vec<String> {
    WalkDir::new(TESTS_DIR)
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.file_type().is_file())
        .filter(|entry| {
            entry
                .path()
                .extension()
                .is_some_and(|s| s.to_str() == Some(EXTENSION))
        })
        .map(|entry| entry.path().to_string_lossy().into_owned())
        .collect()
}

fn read_return_code(expected_results: &Value, source_path: &str) -> Option<i32> {
    expected_results
        .get(source_path)
        .and_then(|v| v.get(RETURN_CODE))
        .and_then(|v| v.as_i64())
        .map(|v| v as i32)
}

fn to_function_name(path: &str) -> String {
    path.replace("/", "_")
        .replace("\\", "_")
        .replace(".", "_")
        .replace("-", "_")
}

fn write_test_case(file: &mut File, name: &str, path: &str, expected_return_code: Option<i32>) {
    writeln!(
        file,
        r##"#[test]
fn test_{name}() -> Result<(), String> {{
    let source_code = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/{}/{}"));"##,
        TESTS_DIR,
        path.replace("\\", "/")
    )
    .unwrap();

    if let Some(expected_return_code) = expected_return_code {
        writeln!(
            file,
            r##"    let return_code = build_and_run(source_code)?;
    assert_eq!(return_code as u8, {expected_return_code}, "Unexpected return code");"##
        )
        .unwrap();
    } else {
        writeln!(
            file,
            r##"    assert!(build_and_run(source_code).is_err());"##
        )
        .unwrap();
    }

    writeln!(
        file,
        r##"    Ok(())
}}
"##
    )
    .unwrap();
}
