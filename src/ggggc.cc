// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#include "jittefex/config.h"

extern "C" {
#include "ggggc/allocator.c"
#include "ggggc/collector/gembc.c"
#include "ggggc/globals.c"
#include "ggggc/roots.c"
#include "ggggc/threads.c"
}
