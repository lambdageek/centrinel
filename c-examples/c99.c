
int foo (int x) {
	foo (x);
	int y = foo (x);
	return foo (y);
}
