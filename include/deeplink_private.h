/* Included only from deeplink.h, no header protector needed */

#ifndef CONCAT
#define CONCAT_(A, B) A ## B
#define CONCAT(A,B) CONCAT_(A,B)
#endif

#define _DEEPLINK__ADD(section_name, ofile)             \
    static struct {                                     \
        char file[sizeof __FILE__];                     \
        char dependency[sizeof ofile];                  \
    } CONCAT(deeplink, __COUNTER__)                     \
    __attribute__ ((used, section(section_name))) =     \
    { __FILE__, (ofile) };

#define _DEEPLINK__ADD_OFILE(ofile)    _DEEPLINK__ADD("deeplink-dep", ofile)
#define _DEEPLINK__ADD_LIB(libname)    _DEEPLINK__ADD("deeplink-dep", "-l" libname)
#define _DEEPLINK__PRUNE_OFILE(ofile)  _DEEPLINK__ADD("deeplink-prune", ofile)
#define _DEEPLINK__PRUNE_LIB(libname)  _DEEPLINK__ADD("deeplink-prune", "-l" libname)
