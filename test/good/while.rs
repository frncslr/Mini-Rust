fn main() {
    println!("{}", simple());
}

fn simple() -> i32 {
    let x: i32;
    x = 0;
    while (x<10) {
        println!("{}", x);
        x = x +1;
    }
    return x;
}
