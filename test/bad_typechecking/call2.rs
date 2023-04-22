fn main() {
    println!("{}", init());
}

fn init() -> i32 {
    return m1(4, 5, 6);
}

fn m1(a: i32, b: i32, c: i32) -> bool {
    return a + b < c;
}
