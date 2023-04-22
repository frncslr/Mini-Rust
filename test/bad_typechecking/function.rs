fn main() {
    println!("{}", init());
}

fn init() -> i32 {
    return 0;
}

// No overloading in Rust and MiniRust
fn m1(a: i32) -> i32 {
    return 0;
}

fn m1() -> i32 {
    return 0;
}
