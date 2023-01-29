// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#ifndef JITTEFEX_GC_H
#define JITTEFEX_GC_H 1

#include "jittefex/config.h"

#ifdef JITTEFEX_ENABLE_GC_STACK
#define GGGGC_FEATURE_JITPSTACK
#endif

#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
#define GGGGC_FEATURE_EXTTAG
#endif

#define GGGGC_FEATURE_FINALIZERS

#include "ggggc/gc.h"

#endif
