// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#ifndef JITTEFEX_COMPILE_H
#define JITTEFEX_COMPILE_H 1

#include "jittefex/config.h"

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
