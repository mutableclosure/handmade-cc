use handmade_cc::Compiler;
use std::path::Path;
use std::process;
use std::{env, fs};

#[cfg(test)]
mod test;

const WAT_ONLY_OPTION: &str = "--wat-only";

fn main() {
    #[cfg(feature = "std")]
    std_logger::Config::logfmt().init();

    let mut args = env::args().collect();
    let wat_only = read_wat_only_option(&mut args);
    let source_path = Path::new(&args[1]);
    let Some(source_file_name) = read_file_name(source_path) else {
        exit_on_invalid_args(&args);
    };
    let source = read_source_file(source_path);
    let output_path = Path::new(&args[2]);
    let wat = compile(&source_file_name, &source);

    if wat_only {
        write_output(output_path, wat);
    } else {
        let binary = wat_to_binary(&wat);
        write_output(output_path, binary);
    }
}

fn compile(file_name: &str, source: &str) -> String {
    Compiler.compile(source).unwrap_or_else(|error| {
        eprintln!("{file_name}:{error}");
        process::exit(1);
    })
}

fn wat_to_binary(wat: &str) -> Vec<u8> {
    #[cfg(feature = "binary-output")]
    {
        Compiler.assemble(wat).unwrap_or_else(|e| {
            eprintln!("Error compiling wat to binary: {e}",);
            process::exit(1);
        })
    }

    #[cfg(not(feature = "binary-output"))]
    {
        eprintln!("Compiler compiled without wat to binary support!");
        process::exit(1);
    }
}

fn write_output(path: &Path, output: impl AsRef<[u8]>) {
    if let Err(error) = fs::write(path, output) {
        eprintln!(
            "Cannot write output file at '{}': {}",
            path.display(),
            error
        );
        process::exit(1);
    }
}

fn read_wat_only_option(args: &mut Vec<String>) -> bool {
    match args.len() {
        3 => false,
        4 => args
            .iter()
            .position(|a| *a == WAT_ONLY_OPTION)
            .map(|i| {
                args.remove(i);
                true
            })
            .unwrap_or_default(),
        _ => exit_on_invalid_args(args),
    }
}

fn read_file_name(path: &Path) -> Option<String> {
    path.file_name().map(|n| n.to_string_lossy().into_owned())
}

fn read_source_file(path: &Path) -> String {
    match fs::read_to_string(path) {
        Ok(source) => source,
        Err(error) => {
            eprintln!("Cannot read source file at '{}': {}", path.display(), error);
            process::exit(1);
        }
    }
}

fn exit_on_invalid_args(args: &[String]) -> ! {
    eprintln!(
        "Usage:\n  {} [--wat-only] <source_file.c> <output.{{wasm | wat}}>",
        args.first().unwrap_or(&"handmade-cc".to_string())
    );
    process::exit(1);
}
