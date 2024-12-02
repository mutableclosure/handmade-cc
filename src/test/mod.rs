use handmade_cc::Compiler;
use wasmi::{Caller, Engine, Func, Linker, Memory, MemoryType, Module, Store};

const MEMORY_PAGES: u32 = 1;
const ENV_MODULE: &str = "env";
const MEMORY_NAME: &str = "memory";
const MAIN_NAME: &str = "main";
const PUT_CHAR_NAME: &str = "putchar";

#[cfg(feature = "std")]
static INIT: std::sync::Once = std::sync::Once::new();

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

fn build_and_run(source_code: &str) -> Result<i32, String> {
    #[cfg(feature = "std")]
    INIT.call_once(|| {
        std_logger::Config::logfmt().init();
    });

    let wat = Compiler
        .compile(source_code)
        .map_err(|error| error.to_string())?;

    println!("{wat}");

    if source_code.lines().next() == Some("// DEBUG") {
        panic!("Stopped for debugging");
    }

    let binary = wat::parse_str(wat).map_err(|error| error.to_string())?;
    let engine = Engine::default();
    let module = Module::new(&engine, &binary).map_err(|error| error.to_string())?;
    let mut store = Store::new(&engine, ());
    let memory_type = MemoryType::new(MEMORY_PAGES, Some(MEMORY_PAGES)).unwrap();
    let memory = Memory::new(&mut store, memory_type).unwrap();
    let mut linker = <Linker<()>>::new(&engine);
    linker.define(ENV_MODULE, MEMORY_NAME, memory).unwrap();
    linker
        .define(ENV_MODULE, PUT_CHAR_NAME, Func::wrap(&mut store, putchar))
        .unwrap();
    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|error| error.to_string())?
        .start(&mut store)
        .map_err(|error| error.to_string())?;
    let main = instance
        .get_typed_func::<(), i32>(&store, MAIN_NAME)
        .map_err(|error| error.to_string())?;
    main.call(&mut store, ()).map_err(|error| error.to_string())
}

fn putchar(_: Caller<'_, ()>, c: i32) -> i32 {
    println!("{c}");
    c
}
