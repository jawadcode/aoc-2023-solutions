use std::{env, fs, process, time::Instant};

mod trebuchet;

fn main() {
    let mut args = env::args();
    let name = args.next().unwrap();
    let (Some(puzzle), Some(path)) = (args.next(), args.next()) else {
        help(&name)
    };
    let content = fs::read_to_string(&path)
        .map_err(|err| {
            eprintln!("Cannot read file {path}: {err}");
            process::exit(1)
        })
        .unwrap();

    let result = match puzzle.to_lowercase().as_str() {
        "1" | "one" => {
            let timer = Instant::now();
            let result = trebuchet::one(content.lines());
            let end = timer.elapsed();
            println!("Part 1 solved in {} micros", end.as_micros());
            result
        }
        "2" | "two" => {
            let timer = Instant::now();
            let result = trebuchet::two(content.lines());
            let end = timer.elapsed();
            println!("Part 1 solved in {} micros", end.as_micros());
            result
        }
        puzzle => {
            eprintln!("Unknown puzzle '{puzzle}'");
            help(&name)
        }
    };

    println!("Result: {result}");
}

fn help(name: &str) -> ! {
    eprintln!("Usage: {name} <puzzle> <path>");
    process::exit(1)
}
