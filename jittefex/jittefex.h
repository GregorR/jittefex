#ifndef JITTEFEX_JITTEFEX_H
#define JITTEFEX_JITTEFEX_H 1

#include "config.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#endif

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
        static llvm::Expected<std::unique_ptr<Jittefex>> create();

#ifdef JITTEFEX_HAVE_LLVM
        const llvm::DataLayout &getDataLayout() const { return dl; }

        llvm::orc::JITDylib &getMainJITDylib() { return mainJD; }

        llvm::Error addModule(
            llvm::orc::ThreadSafeModule tsm, llvm::orc::ResourceTrackerSP rt = nullptr
        );

        llvm::Expected<llvm::JITEvaluatedSymbol> lookup(llvm::StringRef name);
#endif

    private:
        static llvm::Expected<llvm::orc::ThreadSafeModule> optimizeModule(
            llvm::orc::ThreadSafeModule tsm, const llvm::orc::MaterializationResponsibility &r
        );
};

}

#endif
