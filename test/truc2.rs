fn main() {
    println!("{}",simple());
}

fn simple() -> i32 {
    let array : [i32; 3];
    array = [1,2,3];
    return array[1];
}