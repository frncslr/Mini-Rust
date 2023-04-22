fn main() {
    // Illegal lexem '&'
    if (true & true){
        println!("{}",42);
    } else {
        println!("{}",0);
    }
}