// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#define _DEFAULT_SOURCE

#include "jittefex/gc.h"

#undef _DEFAULT_SOURCE

extern "C" {
#include "ggggc/allocator.c"
#define STRINGIFY(x) #x
#define COLLECTOR_F2(x) STRINGIFY(ggggc/collector/x.c)
#define COLLECTOR_F COLLECTOR_F2(GGGGC_COLLECTOR)
#include COLLECTOR_F
#undef COLLECTOR_F
#undef COLLECTOR_F2
#undef STRINGIFY
#include "ggggc/globals.c"
#include "ggggc/roots.c"
#include "ggggc/threads.c"
}
