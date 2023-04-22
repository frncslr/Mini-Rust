fn main() {
    // Should write: 1 < 2 && 2 < 3
    if (1 < 2 < 3) {
        println!("{}", 1);
    } else {
        println!("{}", 0);
    }
}