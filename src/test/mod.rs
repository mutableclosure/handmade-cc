// Copyright (c) 2025 Ludovico Rossi
// SPDX-License-Identifier: MPL-2.0

use handmade_cc::Compiler;
use std::time::Instant;
use wasmi::{AsContext, Caller, Engine, Func, Linker, Memory, MemoryType, Module, Store};

const MEMORY_PAGES: u32 = 1;
const ENV_MODULE: &str = "env";
const MEMORY_NAME: &str = "memory";
const MAIN_NAME: &str = "main";
const PUT_CHAR_NAME: &str = "putchar";
const READ_U8_NAME: &str = "read_u8";
const MAX_EXECUTION_TIME: f64 = 2.0;

#[cfg(feature = "std")]
static INIT: std::sync::Once = std::sync::Once::new();

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

#[derive(Debug, Default)]
struct State {
    stdout: Vec<i32>,
    memory: Option<Memory>,
}

fn build_and_run(source_code: &str) -> Result<(i32, String), String> {
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
    let state = State::default();
    let mut store = Store::new(&engine, state);
    let memory_type = MemoryType::new(MEMORY_PAGES, Some(MEMORY_PAGES)).unwrap();
    let memory = Memory::new(&mut store, memory_type).unwrap();
    let mut linker = <Linker<State>>::new(&engine);
    linker.define(ENV_MODULE, MEMORY_NAME, memory).unwrap();
    store.data_mut().memory = Some(memory);
    linker
        .define(ENV_MODULE, PUT_CHAR_NAME, Func::wrap(&mut store, putchar))
        .unwrap();
    linker
        .define(ENV_MODULE, READ_U8_NAME, Func::wrap(&mut store, read_u8))
        .unwrap();
    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|error| error.to_string())?
        .start(&mut store)
        .map_err(|error| error.to_string())?;
    let main = instance
        .get_typed_func::<(), i32>(&store, MAIN_NAME)
        .map_err(|error| error.to_string())?;

    let start = Instant::now();
    let result = main.call(&mut store, ()).map_err(|error| error.to_string());
    if start.elapsed().as_secs_f64() > MAX_EXECUTION_TIME {
        panic!("Execution took too long!");
    }

    let stdout = String::from_iter(
        store
            .data()
            .stdout
            .iter()
            .map(|c| char::from_u32(*c as u32).unwrap_or_default()),
    );

    result.map(|value| (value, stdout))
}

fn putchar(mut caller: Caller<'_, State>, c: i32) -> i32 {
    print!("{}", char::from_u32(c as u32).unwrap_or_default());
    caller.data_mut().stdout.push(c);
    c
}

fn read_u8(caller: Caller<'_, State>, address: i32) -> i32 {
    let memory = caller.data().memory.unwrap();
    let data = memory.data(caller.as_context());
    data.get(address as usize).copied().unwrap_or_default() as i32
}
