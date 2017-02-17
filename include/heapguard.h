/* HeapGuard include file. */
#ifndef __HEAPGUARD_HEAPGUARD_H
#define __HEAPGUARD_HEAPGUARD_H

/* language-c does not understand Clang's blocks extension */
#undef __BLOCKS__

/* defined when Heapguard is running */
#define __HEAPGUARD__ 1

/* Define to have heapguard define away the GCC legacy __sync atomic operations. 
 * language-c does not currently have the ability to analyze them correctly.
 */
#define __HEAPGUARD_HACK_SYNC_ATOMICS 1

/* attrubte for structs for which heapguard will prevent raw access */
#define __HEAPGUARD_MANAGED_REGION __region(1)

/* attribute specifier for above */
#define __HEAPGUARD_MANAGED_ATTR __attribute__((__HEAPGUARD_MANAGED_REGION))



#ifdef __HEAPGUARD_HACK_SYNC_ATOMICS

/* complete list is here https://gcc.gnu.org/onlinedocs/gcc/_005f_005fsync-Builtins.html */

#define __sync_fetch_and_add(ptr,value,...)  ({ typeof((ptr)) __heapguard_ptr = (ptr);  typeof (*__heapguard_ptr) __heapguard_tmp = *__heapguard_ptr; *__heapguard_ptr += (value); __heapguard_tmp; })
#define __sync_fetch_and_sub(ptr,value,...)  ({ typeof((ptr)) __heapguard_ptr = (ptr);  typeof (*__heapguard_ptr) __heapguard_tmp = *__heapguard_ptr; *__heapguard_ptr -= (value); __heapguard_tmp; })
#define __sync_fetch_and_or(ptr,value,...)   ({ typeof((ptr)) __heapguard_ptr = (ptr);  typeof (*__heapguard_ptr) __heapguard_tmp = *__heapguard_ptr; *__heapguard_ptr |= (value); __heapguard_tmp; })
#define __sync_fetch_and_and(ptr,value,...)  ({ typeof((ptr)) __heapguard_ptr = (ptr);  typeof (*__heapguard_ptr) __heapguard_tmp = *__heapguard_ptr; *__heapguard_ptr &= (value); __heapguard_tmp; })
#define __sync_fetch_and_xor(ptr,value,...)  ({ typeof((ptr)) __heapguard_ptr = (ptr);  typeof (*__heapguard_ptr) __heapguard_tmp = *__heapguard_ptr; *__heapguard_ptr ^= (value); __heapguard_tmp; })
#define __sync_fetch_and_nand(ptr,value,...) ({ typeof((ptr)) __heapguard_ptr = (ptr);  typeof (*__heapguard_ptr) __heapguard_tmp = *__heapguard_ptr; *__heapguard_ptr = ~(__heapguard_tmp & (value)); __heapguard_tmp; })

#define __sync_add_and_fetch(ptr,value,...) (*(ptr) += (value))
#define __sync_sub_and_fetch(ptr,value,...) (*(ptr) -= (value))
#define __sync_or_and_fetch(ptr,value,...) (*(ptr) |= (value))
#define __sync_and_and_fetch(ptr,value,...) (*(ptr) &= (value))
#define __sync_xor_and_fetch(ptr,value,...) (*(ptr) ^= (value))
#define __sync_nand_and_fetch(ptr,value,...) ({ typeof((ptr)) __heapguard_ptr = (ptr); *__heapguard_ptr = ~(*__heapguard_ptr & (value)); *__heapguard_ptr; })

#define __sync_bool_compare_and_swap(ptr,oldval,newval) ({ typeof((ptr)) __heapguard_ptr = (ptr); (*__heapguard_ptr == (oldval)) ? ((*__heapguard_ptr = (newval)), 1) : 0; })

#define __sync_val_compare_and_swap(ptr,oldval,newval) ({ typeof((ptr)) __heapguard_ptr = (ptr); typeof(*__heapguard_ptr) __heapguard_tmp = *__heapguard_ptr; if (*__heapguard_ptr == (oldval)) { *__heapguard_ptr = (newval); }; __heapguard_tmp; })

#define __sync_synchronize(...)

#define __sync_lock_test_and_set(ptr,value) ({ typeof((ptr)) __heapguard_ptr = (ptr); typeof (*__heapguard_ptr) __heapguard_tmp = *__heapguard_ptr; *__heapguard_ptr = (value); __heapguard_tmp; })

#define __sync_lock_release(ptr) do { *(ptr) = 0; } while (0)

#endif /* __HEAPGUARD_HACK_SYNC_ATOMICS */

#endif
