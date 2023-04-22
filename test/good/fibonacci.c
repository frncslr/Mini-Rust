/*

  fn main() {
    println!("{}",fibo(10));
  }
 
fn fibo(let n : i32 ) -> i32 {
  let res : i32 ;
  if (n < 2) {
    res = n;
  }
  else {
    res = fibo(n - 2) + fibo(n - 1);
  }
  return res;
}*/
#include <stdio.h>
#include <stdlib.h>
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"

int fibo(int n) {
  int res;
  if ((n < 2)) {
    res = n;
  }
  else {
    res = (fibo((n - 2)) + fibo((n - 1)));
  }
  return (res);
}
int main(int argc, char *argv[]) {
  printf("%d\n", fibo(10));

  return 0;
}
