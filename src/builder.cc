#include "jittefex/builder.h"

#include "jittefex/compile.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/IRBuilder.h"
#endif

#ifdef JITTEFEX_HAVE_SFJIT
#include "sfjit/sljitLir.h"
#if !(defined SLJIT_CONFIG_UNSUPPORTED && SLJIT_CONFIG_UNSUPPORTED)
#define JITTEFEX_USE_SFJIT 1
#endif
#endif

#include <cassert>

#ifdef DEBUG
#include <iostream>
#endif

#define SJ \
    Function *f = insertionPoint->parent; \
    struct sljit_compiler *sc = (struct sljit_compiler *) f->sljitCompiler; \
    if (sc)
#define SCANCEL() do { \
    insertionPoint->parent->cancelSLJIT(); \
    return ret; \
} while (0)

namespace jittefex {

void IRBuilder::setInsertPoint(BasicBlock *to)
{
#ifdef JITTEFEX_USE_SFJIT
#ifdef DEBUG
    if (insertionPoint) {
        /* Make sure that all instructions in the current block have been
         * released */
        for (auto &inst : insertionPoint->getInstructions()) {
            if (inst->getOpcode() == Opcode::Alloca) {
                // Allocas don't need to be released
                continue;
            }
            if (inst->sljitLoc.reg >= 0) {
                std::cerr << "Instruction of type " <<
                    inst->getOpcode() <<
                    " unreleased!" << std::endl;
                abort();
            }
        }
    }
#endif

    Function *f = to->parent;
    if (f->sljitCompiler) {
        struct sljit_compiler *sc = (struct sljit_compiler *)
            f->sljitCompiler;

        // If this is the initial entry, get all the arguments into place
        if (!f->sljitInit) {
            int argIdx = 0, stype;
            sljit_s32 reg;
            sljit_sw off;

            // Create the basic entry
            stype = f->getFunctionType()->getReturnType().getSLJITType();
            if (stype < 0) {
                // Not compilable with SLJIT!
                f->cancelSLJIT();
        
            } else {
                if (sljit_emit_enter_multiarg(sc, 0, stype,
                    SLJIT_NUMBER_OF_REGISTERS - SLJIT_NUMBER_OF_SAVED_REGISTERS,
                    SLJIT_NUMBER_OF_SAVED_REGISTERS,
                    SLJIT_NUMBER_OF_FLOAT_REGISTERS -
                    SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS,
                    SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS, 0) != 0)
                    f->cancelSLJIT();
            }

            // Load all the arguments
            if (f->sljitCompiler) {
                for (auto &type : f->getFunctionType()->getParamTypes()) {
                    stype = type.getSLJITType();
                    if (stype < 0) {
                        f->cancelSLJIT();
                        break;
                    }
                    if (sljit_emit_get_marg(
                                sc, stype, SLJIT_R(argIdx++), &reg, &off)) {
                        f->cancelSLJIT();
                        break;
                    }
                    f->sljitArgLocs.push_back(SLJITLocation{reg, off});
                }
            }

            // Then get them out of registers
            if (f->sljitCompiler) {
                argIdx = 0;
                for (auto &type : f->getFunctionType()->getParamTypes()) {
                    auto &loc = f->sljitArgLocs[argIdx++];
                    if (loc.reg & SLJIT_MEM)
                        continue;
                    reg = SLJIT_MEM1(SLJIT_FRAMEP);
                    off = -sizeof(sljit_f64) - f->sljitStack.size() * sizeof(sljit_f64);
                    stype = type.getSLJITType();
                    if (stype < SLJIT_ARG_TYPE_F64) {
                        // Word-like
                        if (sljit_emit_op1(sc, SLJIT_MOV,
                            reg, off, loc.reg, loc.off)) {
                            f->cancelSLJIT();
                            break;
                        }
                    } else {
                        // Float-like
                        if (sljit_emit_fop1(sc, SLJIT_MOV_F64,
                            reg, off, loc.reg, loc.off)) {
                            f->cancelSLJIT();
                            break;
                        }
                    }
                    loc.reg = reg;
                    loc.off = off;
                    f->sljitStack.push_back(true);
                }
            }

            // And mark all the registers as free
            for (int i = 0; i < SLJIT_NUMBER_OF_REGISTERS - 1 /* one for our own use */; i++)
                f->sljitRegs.push_back(false);
            for (int i = 0; i < SLJIT_NUMBER_OF_FLOAT_REGISTERS; i++)
                f->sljitFRegs.push_back(false);

            // Make the alloca for later filling
            if (f->sljitCompiler)
                f->sljitAlloca = sljit_emit_alloca(sc, 0);

            f->sljitInit = true;
        }

        // Set this label
        if (f->sljitCompiler) {
            if (!(to->sljitLabel = sljit_emit_label(sc)))
                f->cancelSLJIT();
        }
    }
#endif

    insertionPoint = to;
}

// 966
Instruction *IRBuilder::createRet(
    Instruction *v
) {
    Instruction *ret = insertionPoint->append(
        std::make_unique<RetInst>(insertionPoint, v));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (!v) {
            if (sljit_emit_return_void(sc))
                SCANCEL();

        } else {
            sljit_s32 op;

            switch (v->getType().getSLJITType()) {
                case SLJIT_ARG_TYPE_W:
                    op = SLJIT_MOV;
                    break;

                case SLJIT_ARG_TYPE_32:
                    op = SLJIT_MOV32;
                    break;

                case SLJIT_ARG_TYPE_P:
                    op = SLJIT_MOV_P;
                    break;

                case SLJIT_ARG_TYPE_F64:
                    op = SLJIT_MOV_F64;
                    break;

                case SLJIT_ARG_TYPE_F32:
                    op = SLJIT_MOV_F32;
                    break;

                default:
                    SCANCEL();
            }

            if (sljit_emit_return(sc, op, v->sljitLoc.reg, v->sljitLoc.off))
                SCANCEL();
        }
    }
#endif

    return ret;
}

// 985
Instruction *IRBuilder::createBr(
    BasicBlock *dest
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    return insertionPoint->append(
        std::make_unique<BrInst>(insertionPoint, dest));
}

// 991
Instruction *IRBuilder::createCondBr(
    Instruction *cond, BasicBlock *ifTrue, BasicBlock *ifFalse
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    return insertionPoint->append(
        std::make_unique<BrInst>(insertionPoint, cond, ifTrue, ifFalse));
}

// 1208
Instruction *IRBuilder::createAdd(
    Instruction *l, Instruction *r, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    assert(l->getType() == r->getType());
    return insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::Add, l->getType(),
            l, r)
    );
}

// 1225
Instruction *IRBuilder::createSub(
    Instruction *l, Instruction *r, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    assert(l->getType() == r->getType());
    return insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::Sub, l->getType(),
            l, r)
    );
}

// 1242
Instruction *IRBuilder::createMul(
    Instruction *l, Instruction *r, const std::string &name
) {
    // FIXME
    (void) name;
    assert(l->getType() == r->getType());
    Instruction *ret = insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::Mul, l->getType(),
            l, r)
    );

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        int stype = l->getType().getSLJITType();
        sljit_s32 op;

        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();

        switch (stype) {
            case SLJIT_ARG_TYPE_W:
                op = SLJIT_MUL;
                break;

            case SLJIT_ARG_TYPE_32:
                op = SLJIT_MUL32;
                break;

            default:
                SCANCEL();
        }

        if (sljit_emit_op2(sc, op,
            ret->sljitLoc.reg, ret->sljitLoc.off,
            l->sljitLoc.reg, l->sljitLoc.off,
            r->sljitLoc.reg, r->sljitLoc.off))
            SCANCEL();
    }
#endif

    return ret;
}

// 1423
Instruction *IRBuilder::createFAdd(
    Instruction *l, Instruction *r, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    assert(l->getType() == r->getType());
    return insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::FAdd, l->getType(), l, r));
}

// 1448
Instruction *IRBuilder::createFSub(
    Instruction *l, Instruction *r, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    assert(l->getType() == r->getType());
    return insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::FSub, l->getType(), l, r));
}

// 1473
Instruction *IRBuilder::createFMul(
    Instruction *l, Instruction *r, const std::string &name
) {
    // FIXME
    (void) name;
    assert(l->getType() == r->getType());
    Instruction *ret = insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::FMul, l->getType(), l, r));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        int stype = l->getType().getSLJITType();
        sljit_s32 op;

        switch (stype) {
            case SLJIT_ARG_TYPE_F64:
                op = SLJIT_MUL_F64;
                break;

            case SLJIT_ARG_TYPE_F32:
                op = SLJIT_MUL_F32;
                break;

            default:
                SCANCEL();
        }

        if (!f->sljitAllocateRegister(true, ret->sljitLoc))
            SCANCEL();

        if (sljit_emit_fop2(sc, op, ret->sljitLoc.reg, ret->sljitLoc.off,
            l->sljitLoc.reg, l->sljitLoc.off,
            r->sljitLoc.reg, r->sljitLoc.off))
            SCANCEL();
    }
#endif

    return ret;
}

// 1639
Instruction *IRBuilder::createAlloca(
    const Type &ty, Instruction *arraySize,
    const std::string &name
) {
    // FIXME
    (void) name;
    Instruction *ret = insertionPoint->append(
        std::make_unique<AllocaInst>(insertionPoint, ty, arraySize));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (arraySize) {
            // Not supported
            SCANCEL();
        }

        // Convert it to an sljit type
        int stype = ty.getSLJITType();
        if (stype < 0) {
            SCANCEL();
        } else {
            // Make space for it
            ret->sljitLoc.reg = SLJIT_MEM1(SLJIT_FRAMEP);
            ret->sljitLoc.off = -sizeof(sljit_f64) -
                f->sljitStack.size() * sizeof(sljit_f64);
            f->sljitStack.push_back(true);
        }
    }
#endif

    return ret;
}

// 1656
Instruction *IRBuilder::createLoad(
    const Type &ty, Instruction *ptr, bool isVolatile,
    const std::string &name
) {
    // FIXME
    (void) isVolatile;
    (void) name;
    Instruction *ret =
        insertionPoint->append(std::make_unique<LoadInst>(insertionPoint, ty,
            ptr));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        int stype = ty.getSLJITType();
        bool flt = false;
        sljit_s32 op;

        switch (stype) {
            case SLJIT_ARG_TYPE_W:
                op = SLJIT_MOV;
                break;

            case SLJIT_ARG_TYPE_32:
                op = SLJIT_MOV32;
                break;

            case SLJIT_ARG_TYPE_P:
                op = SLJIT_MOV_P;
                break;

            case SLJIT_ARG_TYPE_F64:
                op = SLJIT_MOV_F64;
                flt = true;
                break;

            case SLJIT_ARG_TYPE_F32:
                op = SLJIT_MOV_F32;
                flt = true;
                break;

            default:
                SCANCEL();
        }

        if (!f->sljitAllocateRegister(flt, ret->sljitLoc))
            SCANCEL();

        if (ptr->getOpcode() == Opcode::Alloca) {
            /* The location is the actual allocated location, rather than a
             * pointer */
            if (flt) {
                if (sljit_emit_fop1(sc, op, ret->sljitLoc.reg, ret->sljitLoc.off,
                    ptr->sljitLoc.reg, ptr->sljitLoc.off)) {
                    SCANCEL();
                }
            } else {
                if (sljit_emit_op1(sc, op, ret->sljitLoc.reg, ret->sljitLoc.off,
                    ptr->sljitLoc.reg, ptr->sljitLoc.off)) {
                    SCANCEL();
                }
            }
        } else {
            /* The location is an actual pointer */
            sljit_emit_op1(sc, op, SLJIT_R0, 0, ptr->sljitLoc.reg, ptr->sljitLoc.off);
            if (flt) {
                if (sljit_emit_fop1(sc, op, ret->sljitLoc.reg, ret->sljitLoc.off,
                    SLJIT_MEM1(SLJIT_R0), 0)) {
                    SCANCEL();
                }
            } else {
                if (sljit_emit_op1(sc, op, ret->sljitLoc.reg, ret->sljitLoc.off,
                    SLJIT_MEM1(SLJIT_R0), 0)) {
                    SCANCEL();
                }
            }
        }

    }
#endif

    return ret;
}

// 1695
Instruction *IRBuilder::createStore(
    Instruction *val, Instruction *ptr, bool isVolatile
) {
    // FIXME
    (void) isVolatile;
    Instruction *ret = insertionPoint->append(
        std::make_unique<StoreInst>(insertionPoint, val, ptr));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        int stype = val->getType().getSLJITType();
        bool flt = false;
        sljit_s32 op;
        switch (stype) {
            case SLJIT_ARG_TYPE_W:
                op = SLJIT_MOV;
                break;

            case SLJIT_ARG_TYPE_32:
                op = SLJIT_MOV32;
                break;

            case SLJIT_ARG_TYPE_P:
                op = SLJIT_MOV_P;
                break;

            case SLJIT_ARG_TYPE_F64:
                op = SLJIT_MOV_F64;
                flt = true;
                break;

            case SLJIT_ARG_TYPE_F32:
                op = SLJIT_MOV_F32;
                flt = true;
                break;

            default:
                SCANCEL();
        }

        if (ptr->getOpcode() == Opcode::Alloca) {
            /* The location is the actual allocated location, rather than a
             * pointer */
            if (flt) {
                if (sljit_emit_fop1(sc, op,
                    ptr->sljitLoc.reg, ptr->sljitLoc.off,
                    val->sljitLoc.reg, val->sljitLoc.off)) {
                    SCANCEL();
                }
            } else {
                if (sljit_emit_op1(sc, op,
                    ptr->sljitLoc.reg, ptr->sljitLoc.off,
                    val->sljitLoc.reg, val->sljitLoc.off)) {
                    SCANCEL();
                }
            }
        } else {
            /* The location is an actual pointer */
            sljit_emit_op1(sc, op, SLJIT_R0, 0, ptr->sljitLoc.reg, ptr->sljitLoc.off);
            if (flt) {
                if (sljit_emit_fop1(sc, op, SLJIT_MEM1(SLJIT_R0), 0,
                    val->sljitLoc.reg, val->sljitLoc.off)) {
                    SCANCEL();
                }
            } else {
                if (sljit_emit_op1(sc, op, SLJIT_MEM1(SLJIT_R0), 0,
                    val->sljitLoc.reg, val->sljitLoc.off)) {
                    SCANCEL();
                }
            }
        }

    }
#endif

    return ret;
}

// 2017
Instruction *IRBuilder::createTrunc(
    Instruction *v, const Type &destTy, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<CastInst>(insertionPoint, Opcode::Trunc, v, destTy));
}

// 2021
Instruction *IRBuilder::createZExt(
    Instruction *v, const Type &destTy, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<CastInst>(insertionPoint, Opcode::ZExt, v, destTy));
}

// 2025
Instruction *IRBuilder::createSExt(
    Instruction *v, const Type &destTy, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<CastInst>(insertionPoint, Opcode::SExt, v, destTy));
}

// 2073
Instruction *IRBuilder::createUIToFP(
    Instruction *val, const Type &destTy, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<CastInst>(insertionPoint, Opcode::UIToFP, val,
            destTy));
}

// 2231
Instruction *IRBuilder::createICmpNE(
    Instruction *l, Instruction *r, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<ICmpInst>(
            insertionPoint, l, r,
            false, true, true, false));
}

// 2259
Instruction *IRBuilder::createICmpSLT(
    Instruction *l, Instruction *r, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<ICmpInst>(
            insertionPoint, l, r,
            true, false, true, false));
}

// 2292
Instruction *IRBuilder::createFCmpONE(
    Instruction *l, Instruction *r, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<FCmpInst>(
            insertionPoint, l, r,
            true, true, true, false));
}

// 2322
Instruction *IRBuilder::createFCmpULT(
    Instruction *l, Instruction *r, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    // FIXME
    (void) name;
    return insertionPoint->append(
        std::make_unique<FCmpInst>(
            insertionPoint, l, r,
            false, false, true, false));
}

// 2391
Instruction *IRBuilder::createCall(
    FunctionType *fTy, Instruction *callee,
    const std::vector<Instruction *> &args,
    const std::string &name
) {
    // FIXME
    (void) name;
    Instruction *ret = insertionPoint->append(
        std::make_unique<CallInst>(insertionPoint, fTy, callee, args));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        struct sljit_marg *marg;
        struct sljit_alloca *alloc;
        SLJITLocation loc;
        sljit_s32 wordR, floatR, stackSpace,
            argI, wordI, floatI, stackI;
        std::unordered_map<sljit_s32, SLJITLocation> regMap;
        std::unordered_map<sljit_s32, SLJITLocation> fregMap;

        // Convert it to a marg
        marg = (struct sljit_marg *) fTy->getSLJITType(sc);
        if (!marg)
            SCANCEL();

        // Save all our scratch registers that are in use
        for (
            int i = SLJIT_NUMBER_OF_SAVED_REGISTERS;
            i < f->sljitRegs.size(); i++
        ) {
            if (f->sljitRegs[i]) {
                if (!f->sljitAllocateStack(loc))
                    SCANCEL();
                if (sljit_emit_op1(sc, SLJIT_MOV, loc.reg, loc.off,
                    SLJIT_S(i), 0))
                    SCANCEL();
                regMap[SLJIT_S(i)] = loc;
            }
        }
        for (
            int i = SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS;
            i < f->sljitFRegs.size(); i++
        ) {
            if (f->sljitFRegs[i]) {
                if (!f->sljitAllocateStack(loc))
                    SCANCEL();
                if (sljit_emit_fop1(sc, SLJIT_MOV_F64, loc.reg, loc.off,
                    SLJIT_FS(i), 0))
                    SCANCEL();
                fregMap[SLJIT_FS(i)] = loc;
            }
        } 

        // Get a location for the result
        if (!f->sljitAllocateRegister(marg->args[0] >= SLJIT_ARG_TYPE_F64,
            ret->sljitLoc))
            SCANCEL();

        // Compile the function
        if (sljit_emit_op1(sc, SLJIT_MOV_P, SLJIT_R0, 0,
            callee->sljitLoc.reg, callee->sljitLoc.off))
            SCANCEL();
        if (sljit_emit_icall(sc, SLJIT_CALL, SLJIT_ARGS1(P, P),
            SLJIT_IMM, (sljit_sw) (void *) jittefex::compile))
            SCANCEL();
        if (sljit_emit_op1(sc, SLJIT_MOV_P,
            SLJIT_S(SLJIT_NUMBER_OF_SAVED_REGISTERS), 0,
            SLJIT_R0, 0))
            SCANCEL();

        // Figure out how many arguments we need
        if (sljit_marg_properties(sc, marg, &wordR, &floatR, &stackSpace))
            SCANCEL();

        // Get them into place
        if (stackSpace) {
            alloc = sljit_emit_alloca(sc, stackSpace);
            if (!alloc)
                SCANCEL();
        }
        argI = wordI = floatI = stackI = 0;
        for (auto &type : fTy->getParamTypes()) {
            sljit_s32 op;
            bool flt = false;
            loc = args[argI++]->sljitLoc;
            int stype = type.getSLJITType();

            switch (stype) {
                case SLJIT_ARG_TYPE_W:
                    op = SLJIT_MOV;
                    break;

                case SLJIT_ARG_TYPE_32:
                    op = SLJIT_MOV32;
                    break;

                case SLJIT_ARG_TYPE_P:
                    op = SLJIT_MOV_P;
                    break;

                case SLJIT_ARG_TYPE_F64:
                    op = SLJIT_MOV_F64;
                    flt = true;
                    break;

                case SLJIT_ARG_TYPE_F32:
                    op = SLJIT_MOV_F32;
                    flt = true;
                    break;

                default:
                    SCANCEL();
            }

            // Check if the location moved to the stack
            if (flt) {
                const auto &it = fregMap.find(loc.reg);
                if (it != fregMap.end())
                    loc = it->second;
            } else {
                const auto &it = regMap.find(loc.reg);
                if (it != regMap.end())
                    loc = it->second;
            }

            // Now move it into place
            if (flt) {
                if (floatI++ >= floatR) {
                    // Goes in memory
                    if (sljit_emit_marg_mov(sc, marg, argI - 1,
                        loc.reg, loc.off))
                        SCANCEL();
                } else {
                    // Goes into a register
                    if (sljit_emit_fop1(sc, op, SLJIT_FR(floatI - 1), 0,
                        loc.reg, loc.off))
                        SCANCEL();
                }
            } else {
                if (wordI++ >= wordR) {
                    // Goes in memory
                    if (sljit_emit_marg_mov(sc, marg, argI - 1,
                        loc.reg, loc.off))
                        SCANCEL();
                } else {
                    // Goes into a register
                    if (sljit_emit_op1(sc, op, SLJIT_R(wordI - 1), 0,
                        loc.reg, loc.off))
                        SCANCEL();
                }
            }
        }

        // Call the actual function
        if (sljit_emit_icall_multiarg(sc, marg,
            SLJIT_S(SLJIT_NUMBER_OF_SAVED_REGISTERS), 0))
            SCANCEL();

        if (stackSpace && sljit_emit_pop(sc, alloc->size))
            SCANCEL();

        // Save the result
        switch (marg->args[0]) {
            case SLJIT_ARG_TYPE_W:
            case SLJIT_ARG_TYPE_32:
            case SLJIT_ARG_TYPE_P:
                if (sljit_emit_op1(sc, SLJIT_MOV,
                    ret->sljitLoc.reg, ret->sljitLoc.off, SLJIT_R0, 0))
                    SCANCEL();
                break;

            case SLJIT_ARG_TYPE_F64:
            case SLJIT_ARG_TYPE_F32:
                if (sljit_emit_fop1(sc, SLJIT_MOV_F64,
                    ret->sljitLoc.reg, ret->sljitLoc.off, SLJIT_FR0, 0))
                    SCANCEL();
                break;

            case SLJIT_ARG_TYPE_VOID:
                // Harmless
                break;

            default:
                SCANCEL();
        }

        // Then restore the registers that we saved
        for (auto &it : regMap) {
            if (sljit_emit_op1(sc, SLJIT_MOV,
                it.first, 0, it.second.reg, it.second.off))
                SCANCEL();
            f->sljitReleaseRegister(false, it.second);
        }
        for (auto &it : fregMap) {
            if (sljit_emit_fop1(sc, SLJIT_MOV_F64,
                it.first, 0, it.second.reg, it.second.off))
                SCANCEL();
            f->sljitReleaseRegister(true, it.second);
        }
    }
#endif

    return ret;
}

Instruction *IRBuilder::createArg(int idx) {
    Instruction *ret = insertionPoint->append(
        std::make_unique<ArgInst>(insertionPoint,
            insertionPoint->parent->getFunctionType()->getParamTypes()[idx],
            idx));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        ret->sljitLoc = f->sljitArgLocs[idx];
    }
#endif

    return ret;
}

Instruction *IRBuilder::createIntLiteral(const Type &ty, long long val) {
    Instruction *ret = insertionPoint->append(
        std::make_unique<LiteralInst>(insertionPoint, ty, val));
    assert(ty.getBaseType() != BaseType::Void &&
           ty.getBaseType() != BaseType::Float);

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        int stype;
        sljit_s32 op;

        stype = ty.getSLJITType();
        if (stype < 0)
            SCANCEL();

        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();

        switch (stype) {
            case SLJIT_ARG_TYPE_W:
                op = SLJIT_MOV;
                break;

            case SLJIT_ARG_TYPE_32:
                op = SLJIT_MOV32;
                break;

            case SLJIT_ARG_TYPE_P:
                op = SLJIT_MOV_P;
                break;

            default:
                SCANCEL();
        }

        if (sljit_emit_op1(sc, op, ret->sljitLoc.reg, ret->sljitLoc.off,
            SLJIT_IMM, (sljit_sw) val))
            SCANCEL();
    }
#endif

    return ret;
}

Instruction *IRBuilder::createFltLiteral(const Type &ty, double val) {
    Instruction *ret = insertionPoint->append(
        std::make_unique<LiteralInst>(insertionPoint, ty, val));
    assert(ty.getBaseType() == BaseType::Float);

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        /* FIXME: SLJIT has to have a better way to get float literals than
         * this? */
        union {
            double dbl;
            int32_t ints[2];
            ssize_t word;
        } literal;
        int stype;
        SLJITLocation tmp;

        literal.dbl = val;

        // Type
        stype = ty.getSLJITType();
        if (stype < 0)
            SCANCEL();

        // Target location
        if (!f->sljitAllocateRegister(true, ret->sljitLoc))
            SCANCEL();

        // Temporary location
        if (!f->sljitAllocateStack(tmp))
            SCANCEL();

        // Store it
#if (defined SLJIT_64BIT_ARCHITECTURE && SLJIT_64BIT_ARCHITECTURE)
        if (sljit_emit_op1(sc, SLJIT_MOV, tmp.reg, tmp.off,
            SLJIT_IMM, literal.word))
            SCANCEL();
#else
        if (sljit_emit_op1(sc, SLJIT_MOV32, tmp.reg, tmp.off,
            SLJIT_IMM, literal.ints[0]))
            SCANCEL();
        if (sljit_emit_op1(sc, SLJIT_MOV32, tmp.reg, tmp.off + 4,
            SLJIT_IMM, literal.ints[1]))
            SCANCEL();
#endif
        if (sljit_emit_fop1(sc,
            (stype == SLJIT_ARG_TYPE_F32)
                ? SLJIT_CONV_F32_FROM_F64
                : SLJIT_MOV_F64,
            ret->sljitLoc.reg, ret->sljitLoc.off, tmp.reg, tmp.off))
            SCANCEL();
    }
#endif

    return ret;
}

Instruction *IRBuilder::createFuncLiteral(Function *val) {
    Instruction *ret = insertionPoint->append(
        std::make_unique<FuncLiteralInst>(insertionPoint, val));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();

        if (sljit_emit_op1(sc, SLJIT_MOV_P,
            ret->sljitLoc.reg, ret->sljitLoc.off,
            SLJIT_IMM, (sljit_sw) (void *) val))
            SCANCEL();
    }
#endif

    return ret;
}

}
