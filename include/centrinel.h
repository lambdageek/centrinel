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
/* FIXME: want size_t return here, but can't include any system
 * header, and language-c internally has size_t as int in the semantic
 * pass, but not in the lexer.  If language-c ever changes how it
 * handles size_t, this may need to change. */
int __bulitin_strlen (const char *s); 
#endif /*__GNUC__*/



#endif
