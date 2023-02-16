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
#include "ggggc/collections/list.c"
#include "ggggc/collections/map.c"
}
