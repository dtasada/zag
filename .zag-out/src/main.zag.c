#include <zag.h>

typedef struct {
  i32 x;
  i32 y;
} MyStruct;

void __zag_MyStruct_setX(MyStruct *self, i32 new_x) { self->x = new_x; }

MyStruct __zag_MyStruct_add(MyStruct lhs, MyStruct rhs) {
  return (MyStruct){
      .x = lhs.x + rhs.x,
      .y = lhs.y + rhs.y,
  };
}

i32 foo() { return 42; }

i32 main() {
  const MyStruct a = (MyStruct){
      .x = 1,
      .y = 1,
  };
  const MyStruct *const b = &a;
  const MyStruct *c = &a;
  MyStruct *d = &a;
  b->x;
  printf("Hello world!\n");
  const i32 c = 420;
}
