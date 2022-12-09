#ifndef JITTEFEX_RUN_H
#define JITTEFEX_RUN_H 1

#include "config.h"

#include <cstdarg>

namespace jittefex {

class Function;

/**
 * Get a callable target for this function. It's up to the user to cast this
 * correctly to actually call it.
 * @param func  Function to compile.
 */
void *compile(Function *func);

}

#endif
