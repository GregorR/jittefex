//===- KaleidoscopeJIT.h - A simple JIT for Kaleidoscope --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Contains a simple JIT definition for use in the kaleidoscope tutorials.
//
//===----------------------------------------------------------------------===//

#ifndef KALEIDOSCOPEJIT_H
#define KALEIDOSCOPEJIT_H

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
#include <memory>

class KaleidoscopeJIT {
    private:
        std::unique_ptr<llvm::orc::ExecutionSession> ES;

        llvm::DataLayout DL;
        llvm::orc::MangleAndInterner Mangle;

        llvm::orc::RTDyldObjectLinkingLayer ObjectLayer;
        llvm::orc::IRCompileLayer CompileLayer;
        llvm::orc::IRTransformLayer OptimizeLayer;

        llvm::orc::JITDylib &MainJD;

    public:
        KaleidoscopeJIT(std::unique_ptr<llvm::orc::ExecutionSession> ES,
                llvm::orc::JITTargetMachineBuilder JTMB, llvm::DataLayout DL)
            : ES(std::move(ES)), DL(std::move(DL)), Mangle(*this->ES, this->DL),
            ObjectLayer(*this->ES,
                    []() { return std::make_unique<llvm::SectionMemoryManager>(); }),
            CompileLayer(*this->ES, ObjectLayer,
                    std::make_unique<llvm::orc::ConcurrentIRCompiler>(std::move(JTMB))),
            OptimizeLayer(*this->ES, CompileLayer, optimizeModule),
            MainJD(this->ES->createBareJITDylib("<main>")) {
                MainJD.addGenerator(
                        cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
                                DL.getGlobalPrefix())));
            }

        ~KaleidoscopeJIT() {
            if (auto Err = ES->endSession())
                ES->reportError(std::move(Err));
        }

        static llvm::Expected<std::unique_ptr<KaleidoscopeJIT>> Create() {
            auto EPC = llvm::orc::SelfExecutorProcessControl::Create();
            if (!EPC)
                return EPC.takeError();

            auto ES = std::make_unique<llvm::orc::ExecutionSession>(std::move(*EPC));

            llvm::orc::JITTargetMachineBuilder JTMB(
                    ES->getExecutorProcessControl().getTargetTriple());

            auto DL = JTMB.getDefaultDataLayoutForTarget();
            if (!DL)
                return DL.takeError();

            return std::make_unique<KaleidoscopeJIT>(std::move(ES), std::move(JTMB),
                    std::move(*DL));
        }

        const llvm::DataLayout &getDataLayout() const { return DL; }

        llvm::orc::JITDylib &getMainJITDylib() { return MainJD; }

        llvm::Error addModule(llvm::orc::ThreadSafeModule TSM, llvm::orc::ResourceTrackerSP RT = nullptr) {
            if (!RT)
                RT = MainJD.getDefaultResourceTracker();

            return OptimizeLayer.add(RT, std::move(TSM));
        }

        llvm::Expected<llvm::JITEvaluatedSymbol> lookup(llvm::StringRef Name) {
            return ES->lookup({&MainJD}, Mangle(Name.str()));
        }

    private:
        static llvm::Expected<llvm::orc::ThreadSafeModule>
            optimizeModule(llvm::orc::ThreadSafeModule TSM, const llvm::orc::MaterializationResponsibility &R) {
                TSM.withModuleDo([](llvm::Module &M) {
                        // Create a function pass manager.
                        auto FPM = std::make_unique<llvm::legacy::FunctionPassManager>(&M);

                        // Add some optimizations.
                        FPM->add(llvm::createInstructionCombiningPass());
                        FPM->add(llvm::createReassociatePass());
                        FPM->add(llvm::createGVNPass());
                        FPM->add(llvm::createCFGSimplificationPass());
                        FPM->doInitialization();

                        // Run the optimizations over all functions in the module being added to
                        // the JIT.
                        for (auto &F : M)
                        FPM->run(F);
                        });

                return std::move(TSM);
            }
};

#endif // KALEIDOSCOPEJIT_H
