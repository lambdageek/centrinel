
#ifndef __CENTRINEL_MANAGED_ATTR
#define __CENTRINEL_MANAGED_ATTR __attribute__((__region(1)))
#endif

struct __CENTRINEL_MANAGED_ATTR X {
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

struct __attribute__((__region(2))) H {
	X x;
};

typedef X *XP;

typedef int (*callback)(XP x);

int foo (XP x);

struct Y* bar (int j, XP x);

int baz (callback f);

#define RT_KNOWN __attribute__((__suppress(1)))

struct XPayload {
	X* raw;
};

typedef struct XPayload *XHandle;

extern RT_KNOWN int legacy_x (X* use);

/* Work around language-c 0.7 issue #32
 * (https://github.com/visq/language-c/issues/32) we can't parse
 * attributes on empty statements yet
 * (https://gcc.gnu.org/onlinedocs/gcc/Attribute-Syntax.html#Statement-Attributes-2).
 * So hang the attribute on a dummy label instead.
 */
#define DUMMY_LABEL_(n) ___label_dummy##n
#define DUMMY_LABEL(n) DUMMY_LABEL_(n):

#define UNSUPPRESS(expr) ({ DUMMY_LABEL(__LINE__) __attribute__((__suppress(0))) ; (expr); })
#define SUPPRESS(expr) ({ DUMMY_LABEL(__LINE__) __attribute__((__suppress(1))) ; (expr); })
#define SAFE_FIELD(handle,field) SUPPRESS(UNSUPPRESS(handle)->raw->field)

int rrrr (XHandle x) {
	return x->raw->a;
}

int rrr (XHandle x) {
	int unsafe_a = x->raw->a;
	return unsafe_a;
}

int foo_safe (XHandle x) {
	int unsafe_a = x->raw->a;
	int also_unsafe_a = ({
		dummy_76: __attribute__((__suppress(1))) ; (x)->raw;
		})->a;
	int safe_a = SAFE_FIELD (x, a);
}
