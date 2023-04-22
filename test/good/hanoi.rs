fn main() {
    println!("{}", hanoi(10, 1, 2, 3));
}

fn hanoi(n: i32, a: i32, b: i32, c: i32) -> i32 {
    let moves: i32;
    if (0 < n) {
        moves = hanoi(n - 1, a, c, b);
        println!("{}", a);
        println!("{}", c);
        println!("{}", 0);
        moves = moves + 1 + hanoi(n - 1, b, c, a);
    } else {
        moves = 0;
    }
    return moves;
}
