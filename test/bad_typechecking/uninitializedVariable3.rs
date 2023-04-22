fn main() {
    println!("{}", run());
}

fn run() -> i32 {
    let a: i32;
    return m(42 + a * 2);
}

fn m(n: i32) -> i32 {
    return n;
}
