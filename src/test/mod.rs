use handmade_cc::Compiler;
use wasmi::{Engine, Linker, Module, Store};

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

fn build_and_run(source_code: &str) -> Result<i32, String> {
    let wat = Compiler
        .compile(source_code)
        .map_err(|error| error.to_string())?;
    let binary = wat::parse_str(wat).map_err(|error| error.to_string())?;
    let engine = Engine::default();
    let module = Module::new(&engine, &binary).map_err(|error| error.to_string())?;
    let mut store = Store::new(&engine, ());
    let linker = <Linker<()>>::new(&engine);
    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|error| error.to_string())?
        .start(&mut store)
        .map_err(|error| error.to_string())?;
    let main = instance
        .get_typed_func::<(), i32>(&store, "main")
        .map_err(|error| error.to_string())?;
    main.call(&mut store, ()).map_err(|error| error.to_string())
}
