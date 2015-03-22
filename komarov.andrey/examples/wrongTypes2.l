int f(int, int);

void g() {
    f(0, 1 == 2);
}

void f(int a, int b) {
    return a + b;
}
