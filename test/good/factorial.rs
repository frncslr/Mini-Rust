fn main() {
    {
        println!("{}", fact(10));
        println!("{}", fact(42));
    }
}

fn fact(num: i32) -> i32 {
    let numAux: i32;
    if (num < 1) {
        numAux = 1;
    } else {
        numAux = num * (fact(num - 1));
    }
    return numAux;
}
