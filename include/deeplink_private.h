/* Included only from deeplink.h, no header protector needed */

#ifndef CONCAT
#define CONCAT_(A, B) A ## B
#define CONCAT(A,B) CONCAT_(A,B)
#endif

#define _DEEPLINK__ADD(ofile)                           \
    static struct {                                     \
        char file[sizeof __FILE__];                     \
        char dependency[sizeof ofile];                  \
    } CONCAT(deeplink, __COUNTER__)                     \
    __attribute__ ((used, section("deeplink"))) =       \
    { __FILE__, (ofile) };

#define _DEEPLINK__ADD_OFILE(ofile)  _DEEPLINK__ADD(ofile)
#define _DEEPLINK__ADD_LIB(libname)  _DEEPLINK__ADD("-l" libname)
