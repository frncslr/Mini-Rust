fn main() {
    println!("{}", run(42));
}

fn run(n: i32) -> i32 {
    let a: i32;
    let b: i32;
    b = 0;
    if (n < 0) {
        a = 1;
    } else {
        b = 1;
    }
    return a;
}
