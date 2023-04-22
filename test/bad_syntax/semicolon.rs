
fn main() {
    println!("{}",42);
}

// Missing semicolon
fn init(a: i32, b:i32) -> i32 {
    let a :i32;
    let b :i32;
    a = 5;
    b = a+1
    return b;
}