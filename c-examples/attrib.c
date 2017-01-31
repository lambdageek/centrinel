
#define MANAGED_HEAP 1

struct __attribute__((__region(MANAGED_HEAP))) X {
	int a;
};

typedef struct X X;

/* managed heap, derived from member */
struct Y {
	X x;
};

typedef struct Y Y;

/* no fixed heap */
struct Z {
	int z;
};

/* also no fixed heap */
struct W {
	Y* py;
};
