fn main() {
    println!("{}", fibo(10));
}

fn fibo(n: i32) -> i32 {
    let res: i32;
    if (n < 2) {
        res = n;
    } else {
        res = fibo(n - 2) + fibo(n - 1);
    }
    return res;
}
