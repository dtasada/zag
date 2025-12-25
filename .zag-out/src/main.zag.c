typedef struct {
    int32_t x;
    int32_t y;
} MyStruct;

void __zag_MyStruct_setX(MyStruct * self, int32_t new_x) {
    self->x = new_x;
}

MyStruct __zag_MyStruct_add(MyStruct lhs, MyStruct rhs) {
    return (MyStruct){
        .x = lhs.x + rhs.x,
        .y = lhs.y + rhs.y,
    };
}

int32_t foo() {
    return 42;
}

int32_t main() {
    const MyStruct a = (MyStruct){
        .x = 1,
        .y = 1,
    };
    const MyStruct * const b = &a;
    const MyStruct * c = &a;
    MyStruct * d = &a;
    b->x;
    const int32_t c = 420;
}

