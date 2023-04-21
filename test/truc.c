/*

  fn main() {
    System.out.println(simple());
  }
 
fn simple() -> i32 {
  i32 x;
  i32 y;
  x = 5;
  y = 10;
  return x + y;
}*/
#include <stdio.h>
#include <stdlib.h>
// #include "tgc.h"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#pragma GCC diagnostic ignored "-Wint-to-pointer-cast"
struct array { int* array; int length; };
// tgc_t gc;
void* simple();
void* simple() {
  int x;
  int y;
  x = 5;
  y = 10;
  return (void*)((x + y));
}
int main(int argc, char *argv[]) {
  printf("%d\n", simple());

  return 0;
}
