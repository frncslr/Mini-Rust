/*

  fn main() {
    println!("{}",false + 1 * 2);
  }
 
*/
#include <stdio.h>
#include <stdlib.h>
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"

int main(int argc, char *argv[]) {
  printf("%d\n", (0 + (1 * 2)));

  return 0;
}
