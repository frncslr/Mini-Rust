fn main() {
    println!("{}", init());
}

fn init() -> i32 {
    return m2(m1(1, 2, 3), 5);
}

fn m1(a: i32, b: i32, c: i32) -> bool {
    return a + b < c;
}

fn m2(a: i32, b: i32) -> i32 {
    return a * b;
}
