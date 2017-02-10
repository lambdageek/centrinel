
#define MANAGED_HEAP 1

typedef struct X X;

/* managed heap, derived from member */
struct Y {
	X x;
};

typedef struct Y Y;

struct __attribute__((__region(MANAGED_HEAP))) X {
	int a;
};

/* no fixed heap */
struct Z {
	int z;
};

/* also no fixed heap */
struct W {
	Y* py;
};

struct __attribute__((__region(2))) H {
	X x;
};

typedef X *XP;

int foo (XP x);

