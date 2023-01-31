// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#ifndef JITTEFEX_JITTEFEX_H
#define JITTEFEX_JITTEFEX_H 1

#include "jittefex/config.h"

// TL Optional doesn't support nothrow
#include <cstdlib>
#include <exception>
#include <functional>
#include <new>
#include <type_traits>
#include <utility>
#define throw if (true) abort(); else
#include "tl/optional.hpp"
#undef throw

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/Mangling.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/IR/DataLayout.h"
#endif

#include <memory>

namespace jittefex {

/**
 * A Jittefex JIT. The main entry point to Jittefex.
 */
class Jittefex {
    private:
#ifdef JITTEFEX_HAVE_LLVM
        // The JIT execution session, to run our code in (LLVM)
        std::unique_ptr<llvm::orc::ExecutionSession> es;

        // Per-platform data layout
        llvm::DataLayout dl;

        // Per-platform name mangler
        llvm::orc::MangleAndInterner mangle;

        // Object/linking layer
        llvm::orc::RTDyldObjectLinkingLayer objectLayer;

        // Compiling layer
        llvm::orc::IRCompileLayer compileLayer;

        // Optimizing layer
        llvm::orc::IRTransformLayer optimizeLayer;

        // The pseudo-dynamic library representing our compiled code
        llvm::orc::JITDylib &mainJD;
#endif

    public:
        // Jittefex can only be created by a "create" call, so this is internal
        Jittefex(
#ifdef JITTEFEX_HAVE_LLVM
                std::unique_ptr<llvm::orc::ExecutionSession> es,
                llvm::orc::JITTargetMachineBuilder jtmb,
                llvm::DataLayout dl
#endif
        );

        ~Jittefex();

        /**
         * Create a Jittefex instance.
         */
        static tl::optional<std::unique_ptr<Jittefex>> create();

#ifdef JITTEFEX_HAVE_LLVM
        inline const llvm::DataLayout &getDataLayout() const { return dl; }

        inline llvm::orc::JITDylib &getMainJITDylib() { return mainJD; }

        llvm::Error addModule(
            llvm::orc::ThreadSafeModule tsm, llvm::orc::ResourceTrackerSP rt = nullptr
        );

        llvm::Expected<llvm::JITEvaluatedSymbol> lookup(llvm::StringRef name);
#endif

    private:
#ifdef JITTEFEX_HAVE_LLVM
        static llvm::Expected<llvm::orc::ThreadSafeModule> optimizeModule(
            llvm::orc::ThreadSafeModule tsm, const llvm::orc::MaterializationResponsibility &r
        );
#endif
};

}

#endif
