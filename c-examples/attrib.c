
typedef struct X X;

/* managed heap, derived from member */
struct Y {
	X x;
};

typedef struct Y Y;

struct __CENTRINEL_MANAGED_ATTR X {
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

typedef int (*callback)(XP x);

int foo (XP x);

struct Y* bar (int j, XP x);

int baz (callback f);
