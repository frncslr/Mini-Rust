fn main() {
    println!("{}", simple());
}

fn simple() -> i32 {
    let x: i32;
    let y: i32;
    x = 5;
    if (x < 7) {
        y = 50;
    } else {
        y = 10;
    }
    return y;
}
