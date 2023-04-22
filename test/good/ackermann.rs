fn main() {
    println!("{}", ackermann(3, 4));
}

fn ackermann(m: i32, n: i32) -> i32 {
    let res: i32;
    if (!(m < 0) && !(0 < m)) {
        res = n + 1;
    } else {
        if (!(n < 0) && !(0 < n)) {
            res = ackermann(m - 1, 1);
        } else {
            res = ackermann(m - 1, ackermann(m, n - 1));
        }
    }
    return res;
}
