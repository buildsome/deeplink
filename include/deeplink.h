/** @file
 * Macros for inserting deeplink section entries.
 * See deeplink's README.md for more information.
 */
#ifndef __DEEPLINK__H_
#define __DEEPLINK__H_

/** Tells deeplink that the current file (usually an .h file) is implemented by
 * the given `ofile` */
#define DEEPLINK__ADD_OFILE(ofile)  \
    _DEEPLINK__ADD_OFILE(ofile)

/** Tells deeplink that the current file (usually an .h file) requires the
 * given `libname` library to be included when linking.
 *
 * Example: to have deeplink add `-lm` to the linkage command line, use:
 *     DEEPLINK__ADD_LIB("m")
 */
#define DEEPLINK__ADD_LIB(libname)  \
    _DEEPLINK__ADD_LIB(libname)

#define DEEPLINK__PRUNE_OFILE(ofile)  \
    _DEEPLINK__PRUNE_OFILE(ofile)

#define DEEPLINK__PRUNE_LIB(libname) \
    _DEEPLINK__PRUNE_LIB(libname)

#include "deeplink_private.h"

#endif
