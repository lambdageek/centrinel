
int foo (int x) {
	foo (x);
	int y = foo (x);
	return foo (y);
}

int bar (int x) {
	for (int y = 0; y < 10; y++) {
	}

	for (long y = 1000; y >= 0; y--) {
	}

	char *y = &x;
	return 0;
}
