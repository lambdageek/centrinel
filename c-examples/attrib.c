
#define MANAGED_HEAP 1

struct __attribute__((__region(MANAGED_HEAP))) X {
	int a;
};

typedef struct X X;

struct Y {
	X x;
};

typedef struct Y Y;
