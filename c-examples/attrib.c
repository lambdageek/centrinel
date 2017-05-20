
typedef struct X X;

/* managed heap, derived from member */
struct Y {
	X x;
};

typedef struct Y Y;

#ifndef __CENTRINEL_MANAGED_ATTR
#define __CENTRINEL_MANAGED_ATTR __attribute__((__region(1)))
#endif

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

#define RT_KNOWN __attribute__((__allow_xregion(1)))

struct RT_KNOWN XPayload {
	X* raw;
};

typedef struct XPayload *XHandle;

#define DUMMY_LABEL(n) ___label_dummy##n:

/* Work around language-c 0.7 issue #32
 * (https://github.com/visq/language-c/issues/32) we can't parse
 * attributes on empty statements yet
 * (https://gcc.gnu.org/onlinedocs/gcc/Attribute-Syntax.html#Statement-Attributes-2).
 * So hang the attribute on a dummy label instead.
 */
#define SAFE_FIELD(handle,field) ({ DUMMY_LABEL(__LINE__) __attribute__((__allow_xregion(1))) ; (handle)->raw->field; })

int foo_safe (XHandle x) {
	int unsafe_a = x->raw->a;
	{
	dummy_56: __attribute__((stmt_attrib)) ; foo (x->raw);
	}
	int safe_a = SAFE_FIELD (x, a);
}
