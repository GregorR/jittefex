// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#include "jittefex/config.h"

#ifdef JITTEFEX_HAVE_SFJIT
extern "C" {
#include "sfjit/sljitLir.c"
}
#endif
