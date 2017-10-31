/* Centrinel include file. */
#ifndef __CENTRINEL_CENTRINEL_H
#define __CENTRINEL_CENTRINEL_H

/* language-c does not understand Clang's blocks extension */
#undef __BLOCKS__

/* defined when Centrinel is running */
#define __CENTRINEL__ 1

/* Define to have centrinel define away the GCC legacy __sync atomic operations. 
 * language-c does not currently have the ability to analyze them correctly.
 */
#define __CENTRINEL_HACK_SYNC_ATOMICS 1

/* attrubte for structs for which centrinel will prevent raw access */
#define __CENTRINEL_MANAGED_REGION __region(1)

/* attribute specifier for above */
#define __CENTRINEL_MANAGED_ATTR __attribute__((__CENTRINEL_MANAGED_REGION))

#ifdef __CENTRINEL_HACK_SYNC_ATOMICS

/* complete list is here https://gcc.gnu.org/onlinedocs/gcc/_005f_005fsync-Builtins.html */

#define __sync_fetch_and_add(ptr,value,...)  ({ typeof((ptr)) __centrinel_ptr = (ptr);  typeof (*__centrinel_ptr) __centrinel_tmp = *__centrinel_ptr; *__centrinel_ptr += (value); __centrinel_tmp; })
#define __sync_fetch_and_sub(ptr,value,...)  ({ typeof((ptr)) __centrinel_ptr = (ptr);  typeof (*__centrinel_ptr) __centrinel_tmp = *__centrinel_ptr; *__centrinel_ptr -= (value); __centrinel_tmp; })
#define __sync_fetch_and_or(ptr,value,...)   ({ typeof((ptr)) __centrinel_ptr = (ptr);  typeof (*__centrinel_ptr) __centrinel_tmp = *__centrinel_ptr; *__centrinel_ptr |= (value); __centrinel_tmp; })
#define __sync_fetch_and_and(ptr,value,...)  ({ typeof((ptr)) __centrinel_ptr = (ptr);  typeof (*__centrinel_ptr) __centrinel_tmp = *__centrinel_ptr; *__centrinel_ptr &= (value); __centrinel_tmp; })
#define __sync_fetch_and_xor(ptr,value,...)  ({ typeof((ptr)) __centrinel_ptr = (ptr);  typeof (*__centrinel_ptr) __centrinel_tmp = *__centrinel_ptr; *__centrinel_ptr ^= (value); __centrinel_tmp; })
#define __sync_fetch_and_nand(ptr,value,...) ({ typeof((ptr)) __centrinel_ptr = (ptr);  typeof (*__centrinel_ptr) __centrinel_tmp = *__centrinel_ptr; *__centrinel_ptr = ~(__centrinel_tmp & (value)); __centrinel_tmp; })

#define __sync_add_and_fetch(ptr,value,...) (*(ptr) += (value))
#define __sync_sub_and_fetch(ptr,value,...) (*(ptr) -= (value))
#define __sync_or_and_fetch(ptr,value,...) (*(ptr) |= (value))
#define __sync_and_and_fetch(ptr,value,...) (*(ptr) &= (value))
#define __sync_xor_and_fetch(ptr,value,...) (*(ptr) ^= (value))
#define __sync_nand_and_fetch(ptr,value,...) ({ typeof((ptr)) __centrinel_ptr = (ptr); *__centrinel_ptr = ~(*__centrinel_ptr & (value)); *__centrinel_ptr; })

#define __sync_bool_compare_and_swap(ptr,oldval,newval) ({ typeof((ptr)) __centrinel_ptr = (ptr); (*__centrinel_ptr == (oldval)) ? ((*__centrinel_ptr = (newval)), 1) : 0; })

#define __sync_val_compare_and_swap(ptr,oldval,newval) ({ typeof((ptr)) __centrinel_ptr = (ptr); typeof(*__centrinel_ptr) __centrinel_tmp = *__centrinel_ptr; if (*__centrinel_ptr == (oldval)) { *__centrinel_ptr = (newval); }; __centrinel_tmp; })

#define __sync_synchronize(...)

#define __sync_lock_test_and_set(ptr,value) ({ typeof((ptr)) __centrinel_ptr = (ptr); typeof (*__centrinel_ptr) __centrinel_tmp = *__centrinel_ptr; *__centrinel_ptr = (value); __centrinel_tmp; })

#define __sync_lock_release(ptr) do { *(ptr) = 0; } while (0)

#endif /* __CENTRINEL_HACK_SYNC_ATOMICS */

/* GCC has a ton of builtins that language-c doesn't know about, which
 * causes its semantic analysis to error out.  Quick and dirty hack to
 * predeclare the builtins that are able to have reasonable
 * definitions.
 */
#ifdef __GNUC__
typedef __int128 __int128_t;
typedef __int128 int128_t;
typedef unsigned __int128 __uint128_t;

int __builtin_ctzl (unsigned long l);
int __builtin_ffsll (long long ll);
int __builtin_isnan (double d);
int __builtin_isfinite (double d);
/* https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html says this function is
 * varargs although it only really takes a single argument
 * int __builtin_isinf_sign (...);
 * but language-c doesn't like varargs functions with 0 normal parameters.
 */
int __builtin_isinf_sign (double d);
int __builtin_popcount (unsigned int i);
int __builtin_popcountll (unsigned long long ll);
int __builtin_signbit (double d);
int __builtin_signbitf (float f);
int __builtin_signbitl (long double ld);
/* FIXME: want size_t return here, but can't include any system
 * header, and language-c internally has size_t as int in the semantic
 * pass, but not in the lexer.  If language-c ever changes how it
 * handles size_t, this may need to change. */
int __builtin_strlen (const char *s);
void __builtin_unreachable (void);
void __builtin_unwind_init (void);
#endif /*__GNUC__*/

#endif
