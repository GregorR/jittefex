// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#include "jittefex/jit.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#endif

namespace jittefex {

// Used internally for global initialization
class JittefexInit {
    public:
    JittefexInit() {
#ifdef JITTEFEX_HAVE_LLVM
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();
#endif
    }
};

static JittefexInit jittefexInit;

Jittefex::Jittefex(
#ifdef JITTEFEX_HAVE_LLVM
    std::unique_ptr<llvm::orc::ExecutionSession> es,
    llvm::orc::JITTargetMachineBuilder jtmb,
    llvm::DataLayout dl
#endif
)
#ifdef JITTEFEX_HAVE_LLVM
    :
    es(std::move(es)),
    dl(std::move(dl)),
    mangle(*this->es, this->dl),
    objectLayer(*this->es, []() {
        return std::make_unique<llvm::SectionMemoryManager>();
    }),
    compileLayer(*this->es, objectLayer,
            std::make_unique<llvm::orc::ConcurrentIRCompiler>(std::move(jtmb))),
    optimizeLayer(*this->es, compileLayer, optimizeModule),
    mainJD(this->es->createBareJITDylib("<jittefex>"))
#endif
{
#ifdef JITTEFEX_HAVE_LLVM
    mainJD.addGenerator(cantFail(
        llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
            dl.getGlobalPrefix())));
#endif
}

Jittefex::~Jittefex() {
#ifdef JITTEFEX_HAVE_LLVM
    if (auto err = es->endSession())
        es->reportError(std::move(err));
#endif
}

tl::optional<std::unique_ptr<Jittefex>> Jittefex::create() {
#ifdef JITTEFEX_HAVE_LLVM
    auto epc = llvm::orc::SelfExecutorProcessControl::Create();
    if (!epc)
        return tl::nullopt;

    auto es = std::make_unique<llvm::orc::ExecutionSession>(std::move(*epc));

    llvm::orc::JITTargetMachineBuilder jtmb(
        es->getExecutorProcessControl().getTargetTriple());

    auto dl = jtmb.getDefaultDataLayoutForTarget();
    if (!dl)
        return tl::nullopt;

    return std::make_unique<Jittefex>(
        std::move(es), std::move(jtmb), std::move(*dl));

#else
    return std::make_unique<Jittefex>();

#endif
}

#ifdef JITTEFEX_HAVE_LLVM
llvm::Error Jittefex::addModule(
    llvm::orc::ThreadSafeModule tsm, llvm::orc::ResourceTrackerSP rt
) {
    if (!rt)
        rt = mainJD.getDefaultResourceTracker();

    return optimizeLayer.add(rt, std::move(tsm));
}

llvm::Expected<llvm::JITEvaluatedSymbol> Jittefex::lookup(llvm::StringRef name) {
    return es->lookup({&mainJD}, mangle(name.str()));
}

llvm::Expected<llvm::orc::ThreadSafeModule> Jittefex::optimizeModule(
    llvm::orc::ThreadSafeModule tsm, const llvm::orc::MaterializationResponsibility &r
) {
    tsm.withModuleDo([](llvm::Module &m) {
        // Create a function pass manager.
        auto fpm = std::make_unique<llvm::legacy::FunctionPassManager>(&m);

        // Add some optimizations.
        fpm->add(llvm::createInstructionCombiningPass());
        fpm->add(llvm::createReassociatePass());
        fpm->add(llvm::createGVNPass());
        fpm->add(llvm::createCFGSimplificationPass());
        fpm->doInitialization();

        // Run the optimizations over all functions in the module being added to
        // the JIT.
        for (auto &f : m)
            fpm->run(f);
    });

    return std::move(tsm);
}
#endif

}
