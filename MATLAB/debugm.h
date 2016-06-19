#ifndef _DEBUGM_H
#define _DEBUGM_H

/* Some debug macros. */

#ifdef NDEBUG
#define debug(M, ...) 
#else
#define debug(M, ...) fprintf( stderr, "[DEBUG] %s:%u: " M "\n" \
                             , __func__, __LINE__, ##__VA_ARGS__)
#endif

#define error(E, M, ...) { fprintf( stderr, "[ERROR] %s:%u: " M "\n"    \
                                  , __func__, __LINE__, ##__VA_ARGS__); \
                           return (E); }

#define check_n(A, E, M, ...) if ((A) < 0) { error(E, M, ##__VA_ARGS__) }
#endif
