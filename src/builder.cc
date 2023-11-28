// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#include "jittefex/builder.h"

#include "jittefex/compile.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/IRBuilder.h"
#endif

#ifdef JITTEFEX_HAVE_SFJIT
#include "sfjit/sljitLir.h"
#if !(defined SLJIT_CONFIG_UNSUPPORTED && SLJIT_CONFIG_UNSUPPORTED)
#define JITTEFEX_USE_SFJIT 1
#define SLJIT_GCSP SLJIT_S0
#define SLJIT_GCFP SLJIT_S1
#endif
#endif

#include <cassert>

#ifdef JITTEFEX_ENABLE_DEBUG
#define NAME , name
#else
#define NAME
#endif

#ifdef JITTEFEX_HAVE_SFJIT
#define SJ \
    Function *f = insertionPoint->parent; \
    struct sljit_compiler *sc = (struct sljit_compiler *) f->sljitCompiler; \
    if (sc)
#define SCANCEL() do { \
    insertionPoint->parent->cancelSLJIT(); \
    return ret; \
} while (0)
#endif

namespace jittefex {

void IRBuilder::setInsertPoint(BasicBlock *to)
{
#ifdef JITTEFEX_USE_SFJIT
    Function *f = to->parent;
    if (f->sljitCompiler) {
        struct sljit_compiler *sc = (struct sljit_compiler *)
            f->sljitCompiler;

        // If this is the initial entry, get all the arguments into place
        if (!f->sljitInit) {
            int argIdx = 0, stype;
            sljit_s32 reg;
            sljit_sw off;
#ifdef JITTEFEX_ENABLE_GC_STACK
            SLJITLocation gcStackLoc;
#endif

            // Create the basic entry
            Type returnType = f->getFunctionType()->getReturnType();
            if (returnType.getBaseType() == BaseType::GCPointer ||
                returnType.getBaseType() == BaseType::TaggedWord) {
                // FIXME: Handled specially
                stype = SLJIT_ARG_TYPE_W;
            } else {
                stype = returnType.getSLJITType();
            }

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
#ifdef JITTEFEX_ENABLE_GC_STACK
                sljit_sw gcCt = 0;
                sljit_sw gcOff =
#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
                    sizeof(sljit_sw)
#else
                    0
#endif
                    ;
                reg = SLJIT_GCSP;
                off = 0;
                sljit_emit_get_marg(
                    sc, SLJIT_ARG_TYPE_P, reg, &gcStackLoc.reg, &gcStackLoc.off);
#endif

                for (auto &type : f->getFunctionType()->getParamTypes()) {
#ifdef JITTEFEX_ENABLE_GC_STACK
                    if (type.getBaseType() == BaseType::GCPointer ||
                        type.getBaseType() == BaseType::TaggedWord) {
                        /* In the GC stack */
                        f->sljitArgLocs.push_back(
                            SLJITLocation{SLJIT_MEM1(SLJIT_GCFP), gcOff});
                        gcCt++;
                        gcOff += sizeof(sljit_sw);
#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
                        if (gcCt % sizeof(sljit_sw) == 0) {
                            // Word for tags here
                            gcOff += sizeof(sljit_sw);
                        }
#endif
                        continue;
                    }
#endif

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

            // Mark all the registers as free
            for (int i = 0; i < SLJIT_NUMBER_OF_REGISTERS - 1 /* one for our own use */; i++)
                f->sljitRegs.push_back(false);
            for (int i = 0; i < SLJIT_NUMBER_OF_FLOAT_REGISTERS; i++)
                f->sljitFRegs.push_back(false);

            // Make the alloca for later filling
            if (f->sljitCompiler)
                f->sljitAlloca = sljit_emit_alloca(sc, 0);

            // Get the args out of registers
            if (f->sljitCompiler) {
                argIdx = 0;
                for (auto &type : f->getFunctionType()->getParamTypes()) {
#ifdef JITTEFEX_ENABLE_GC_STACK
                    if (type.getBaseType() == BaseType::GCPointer ||
                        type.getBaseType() == BaseType::TaggedWord) {
                        // Handled later
                        argIdx++;
                        continue;
                    }
#endif

                    auto &loc = f->sljitArgLocs[argIdx++];
                    if (loc.reg & SLJIT_MEM)
                        continue;

                    // Get a new stack location
                    SLJITLocation newLoc;
                    if (!f->sljitAllocateStack(newLoc)) {
                        f->cancelSLJIT();
                        break;
                    }

                    // Move it there
                    if (stype < SLJIT_ARG_TYPE_F64) {
                        // Word-like
                        if (sljit_emit_op1(sc, SLJIT_MOV,
                            newLoc.reg, newLoc.off, loc.reg, loc.off)) {
                            f->cancelSLJIT();
                            break;
                        }
                    } else {
                        // Float-like
                        if (sljit_emit_fop1(sc, SLJIT_MOV_F64,
                            newLoc.reg, newLoc.off, loc.reg, loc.off)) {
                            f->cancelSLJIT();
                            break;
                        }
                    }
                    loc = newLoc;
                }
            }

#ifdef JITTEFEX_ENABLE_GC_STACK
            if (f->sljitCompiler) {
                /* Move the JIT stack pointer to a saved location (if it's not
                 * already) */
                if (gcStackLoc.reg != SLJIT_GCSP) {
                    if (sljit_emit_op1(sc, SLJIT_MOV, SLJIT_GCSP, 0,
                                gcStackLoc.reg, gcStackLoc.off))
                        f->cancelSLJIT();
                }
                f->sljitRegs[0 /* SP */] = true;
            }

            // Get the GC stack pointer as a frame pointer
            if (f->sljitCompiler) {
                if (sljit_emit_op1(sc, SLJIT_MOV, SLJIT_GCFP, 0, SLJIT_GCSP, 0))
                    f->cancelSLJIT();
                f->sljitRegs[1 /* FP */] = true;
            }

            // Alloca on the GC stack
            if (f->sljitCompiler) {
                f->sljitGCAllocaOut = (void *) sljit_emit_jump(sc, SLJIT_JUMP);
                if (!f->sljitGCAllocaOut)
                    f->cancelSLJIT();
            }
            if (f->sljitCompiler) {
                f->sljitGCAllocaIn = (void *) sljit_emit_label(sc);
                if (!f->sljitGCAllocaIn)
                    f->cancelSLJIT();
            }
#endif

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
    if (v)
        assert(v->getType().isManifest());

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
    Instruction *ret = insertionPoint->append(
        std::make_unique<BrInst>(insertionPoint, dest));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        struct sljit_jump *sj;

        if (!(sj = sljit_emit_jump(sc, SLJIT_JUMP)))
            SCANCEL();

        dest->sljitLabelReqs.push_back((void *) sj);
    }
#endif

    return ret;
}

// 991
Instruction *IRBuilder::createCondBr(
    Instruction *cond, BasicBlock *ifTrue, BasicBlock *ifFalse
) {
    assert(cond->getType().isInteger());
    Instruction *ret = insertionPoint->append(
        std::make_unique<BrInst>(insertionPoint, cond, ifTrue, ifFalse));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        struct sljit_jump *tj, *fj;

        if (!(tj = sljit_emit_cmp(sc, SLJIT_NOT_EQUAL,
            cond->sljitLoc.reg, cond->sljitLoc.off, SLJIT_IMM, 0)))
            SCANCEL();

        if (!(fj = sljit_emit_jump(sc, SLJIT_JUMP)))
            SCANCEL();

        ifTrue->sljitLabelReqs.push_back((void *) tj);
        ifFalse->sljitLabelReqs.push_back((void *) fj);
    }
#endif

    return ret;
}

#ifdef JITTEFEX_USE_SFJIT
static inline bool simath(
    struct sljit_compiler *sc, const Type &type, sljit_s32 op,
    const SLJITLocation &res, const SLJITLocation &l, const SLJITLocation &r
) {
    (void) type;

#if (defined SLJIT_64BIT_ARCHITECTURE && SLJIT_64BIT_ARCHITECTURE)
    if (type.getSLJITType() == SLJIT_ARG_TYPE_32)
        op |= SLJIT_32;
#endif

    if (sljit_emit_op2(sc, op, res.reg, res.off, l.reg, l.off, r.reg, r.off))
        return false;

    return true;
}
#endif

// 1208
Instruction *IRBuilder::createAdd(
    Instruction *l, Instruction *r, const std::string &name
) {
    (void) name;
    assert(l->getType() == r->getType() && l->getType().isInteger());
    Instruction *ret = insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::Add, l->getType(),
            l, r NAME)
    );

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();
        if (!simath(sc, l->getType(), SLJIT_ADD, ret->sljitLoc, l->sljitLoc, r->sljitLoc))
            SCANCEL();
    }
#endif

    return ret;
}

// 1225
Instruction *IRBuilder::createSub(
    Instruction *l, Instruction *r, const std::string &name
) {
    (void) name;
    assert(l->getType() == r->getType() && l->getType().isInteger());
    Instruction *ret = insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::Sub, l->getType(),
            l, r NAME)
    );

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();
        if (!simath(sc, l->getType(), SLJIT_SUB, ret->sljitLoc, l->sljitLoc, r->sljitLoc))
            SCANCEL();
    }
#endif

    return ret;
}

// 1242
Instruction *IRBuilder::createMul(
    Instruction *l, Instruction *r, const std::string &name
) {
    (void) name;
    assert(l->getType() == r->getType() && l->getType().isInteger());
    Instruction *ret = insertionPoint->append(
        std::make_unique<BinaryInst>(insertionPoint, Opcode::Mul, l->getType(),
            l, r NAME)
    );

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();
        if (!simath(sc, l->getType(), SLJIT_MUL, ret->sljitLoc, l->sljitLoc, r->sljitLoc))
            SCANCEL();
    }
#endif

    return ret;
}

#ifdef JITTEFEX_USE_SFJIT
static inline bool sfmath(
    struct sljit_compiler *sc, const Type &type, sljit_s32 op,
    const SLJITLocation &res, const SLJITLocation &l, const SLJITLocation &r
) {
    int stype = type.getSLJITType();
    if (stype == SLJIT_ARG_TYPE_F32)
        op |= SLJIT_32;
    else if (stype != SLJIT_ARG_TYPE_F64)
        return false;

    if (sljit_emit_fop2(sc, op, res.reg, res.off, l.reg, l.off, r.reg, r.off))
        return false;

    return true;
}
#endif

// 1423
Instruction *IRBuilder::createFAdd(
    Instruction *l, Instruction *r, const std::string &name
) {
    (void) name;
    assert(l->getType() == r->getType() &&
        l->getType().getBaseType() == BaseType::Float);
    Instruction *ret = insertionPoint->append(
        std::make_unique<BinaryInst>(
            insertionPoint, Opcode::FAdd, l->getType(), l, r NAME));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (!f->sljitAllocateRegister(true, ret->sljitLoc))
            SCANCEL();
        if (!sfmath(sc, l->getType(), SLJIT_ADD_F64, ret->sljitLoc, l->sljitLoc,
            r->sljitLoc))
            SCANCEL();
    }
#endif

    return ret;
}

// 1448
Instruction *IRBuilder::createFSub(
    Instruction *l, Instruction *r, const std::string &name
) {
    (void) name;
    assert(l->getType() == r->getType() &&
        l->getType().getBaseType() == BaseType::Float);
    Instruction *ret = insertionPoint->append(
        std::make_unique<BinaryInst>(
            insertionPoint, Opcode::FSub, l->getType(), l, r NAME));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (!f->sljitAllocateRegister(true, ret->sljitLoc))
            SCANCEL();
        if (!sfmath(sc, l->getType(), SLJIT_SUB_F64, ret->sljitLoc, l->sljitLoc,
            r->sljitLoc))
            SCANCEL();
    }
#endif

    return ret;
}

// 1473
Instruction *IRBuilder::createFMul(
    Instruction *l, Instruction *r, const std::string &name
) {
    (void) name;
    assert(l->getType() == r->getType() &&
        l->getType().getBaseType() == BaseType::Float);
    Instruction *ret = insertionPoint->append(
        std::make_unique<BinaryInst>(
            insertionPoint, Opcode::FMul, l->getType(), l, r NAME));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (!f->sljitAllocateRegister(true, ret->sljitLoc))
            SCANCEL();
        if (!sfmath(sc, l->getType(), SLJIT_MUL_F64, ret->sljitLoc, l->sljitLoc,
            r->sljitLoc))
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
    (void) name;
    Instruction *ret = insertionPoint->append(
        std::make_unique<AllocaInst>(insertionPoint, ty, arraySize NAME));
#ifdef JITTEFEX_ENABLE_GC_STACK
    assert(ty.getBaseType() != BaseType::GCPointer &&
           ty.getBaseType() != BaseType::TaggedWord);
#endif

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

#ifdef JITTEFEX_ENABLE_DEBUG
    const Type &pType = ptr->getType();
    assert(pType.getBaseType() == BaseType::Pointer ||
           pType.getBaseType() == BaseType::Stack);
#endif

    Instruction *ret =
        insertionPoint->append(std::make_unique<LoadInst>(
            insertionPoint, ty, ptr NAME));

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

        if (ptr->getType().getBaseType() == BaseType::Stack) {
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

#ifdef JITTEFEX_ENABLE_DEBUG
    const Type &pType = ptr->getType();
    assert(pType.getBaseType() == BaseType::Pointer ||
           pType.getBaseType() == BaseType::Stack);
#endif

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

        if (ptr->getType().getBaseType() == BaseType::GCStack) {
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

#ifdef JITTEFEX_ENABLE_GC
#ifdef JITTEFEX_ENABLE_GC_STACK
Instruction *IRBuilder::createGCArg(const Type &ty, int idx) {
    Instruction *ret = insertionPoint->append(
        std::make_unique<GCArgInst>(insertionPoint, ty, idx));
    assert(ty.getBaseType() == BaseType::GCPointer ||
           ty.getBaseType() == BaseType::TaggedWord);

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        SLJITLocation argLoc;

        argLoc.reg = SLJIT_MEM1(SLJIT_GCFP);

#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
        // Account for tag words
        sljit_sw blockNo = idx / sizeof(size_t);
        sljit_sw offsetWithinBlock = idx % sizeof(size_t);
        argLoc.off =
            ((blockNo * (sizeof(size_t) + 1)) + offsetWithinBlock + 1) *
            sizeof(size_t);
        argLoc.tag =
            blockNo * (sizeof(size_t) + 1) * sizeof(size_t) +
            offsetWithinBlock;
#else
        argLoc.off = idx * sizeof(size_t);
#endif

        if (!f->sljitAllocateGCStack(ret->sljitLoc))
            SCANCEL();
        if (sljit_emit_op1(sc, SLJIT_MOV,
            ret->sljitLoc.reg, ret->sljitLoc.off,
            argLoc.reg, argLoc.off))
            SCANCEL();

#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
        if (sljit_emit_op1(sc, SLJIT_MOV_U8,
            ret->sljitLoc.reg, ret->sljitLoc.tag,
            argLoc.reg, argLoc.tag))
            SCANCEL();
#endif
    }
#endif

    return ret;
}

Instruction *IRBuilder::createGCAlloca(
    const Type &ty, Instruction *arraySize,
    const std::string &name
) {
    (void) name;
    Instruction *ret = insertionPoint->append(
        std::make_unique<GCAllocaInst>(insertionPoint, ty, arraySize NAME));
    assert(ty.getBaseType() == BaseType::GCPointer ||
           ty.getBaseType() == BaseType::TaggedWord);

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (arraySize) {
            // Not supported
            SCANCEL();
        }

        // Convert it to an sljit type
        // Make space for it
        if (!f->sljitAllocateGCStack(ret->sljitLoc))
            SCANCEL();
    }
#endif

    return ret;
}
#endif

Instruction *IRBuilder::createGCLoad(
    const Type &ty, Instruction *ptr, int64_t offset, bool isVolatile,
    const std::string &name
) {
    // FIXME
    (void) isVolatile;
    (void) name;

#ifdef JITTEFEX_ENABLE_DEBUG
    const Type &pType = ptr->getType();
    assert(pType.getBaseType() == BaseType::GCPointer ||
           pType.getBaseType() == BaseType::TaggedWord ||
           pType.getBaseType() == BaseType::GCStack);
    if (pType.getBaseType() == BaseType::GCStack)
        assert(offset == 0);
#endif

    Instruction *ret =
        insertionPoint->append(std::make_unique<GCLoadInst>(
            insertionPoint, ty, ptr, offset NAME));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        int stype = ty.getSLJITType(true);
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

#ifdef JITTEFEX_ENABLE_GC_STACK
        if (ty.getBaseType() == BaseType::GCPointer ||
            ty.getBaseType() == BaseType::TaggedWord) {
            // Needs to go on the GC stack
            if (!f->sljitAllocateGCStack(ret->sljitLoc))
                SCANCEL();
        } else
#endif
        if (!f->sljitAllocateRegister(flt, ret->sljitLoc))
            SCANCEL();

        if (ptr->getType().getBaseType() == BaseType::GCStack) {
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
#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
            if (ty.getBaseType() == BaseType::GCPointer ||
                ty.getBaseType() == BaseType::TaggedWord) {
                if (sljit_emit_op1(sc, SLJIT_MOV_U8, ret->sljitLoc.reg,
                    ret->sljitLoc.tag, ptr->sljitLoc.reg, ptr->sljitLoc.tag)) {
                    SCANCEL();
                }
            }
#endif

        } else {
            /* The location is an actual pointer */
            sljit_emit_op1(sc, op, SLJIT_R0, 0, ptr->sljitLoc.reg, ptr->sljitLoc.off);
            if (flt) {
                if (sljit_emit_fop1(sc, op, ret->sljitLoc.reg, ret->sljitLoc.off,
                    SLJIT_MEM1(SLJIT_R0), offset)) {
                    SCANCEL();
                }
            } else {
                if (sljit_emit_op1(sc, op, ret->sljitLoc.reg, ret->sljitLoc.off,
                    SLJIT_MEM1(SLJIT_R0), offset)) {
                    SCANCEL();
                }
            }

#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
            /* Don't get a tag just from this */
            if (ty.getBaseType() == BaseType::GCPointer) {
                if (sljit_emit_op1(sc, SLJIT_MOV_U8, ret->sljitLoc.reg,
                    ret->sljitLoc.tag, SLJIT_IMM, 0)) {
                    SCANCEL();
                }
            } else if (ty.getBaseType() == BaseType::TaggedWord) {
                if (sljit_emit_op1(sc, SLJIT_MOV_U8, ret->sljitLoc.reg,
                    ret->sljitLoc.tag, SLJIT_IMM, 1)) {
                    SCANCEL();
                }
            }
#endif
        }

    }
#endif

    return ret;
}


Instruction *IRBuilder::createGCStore(
    Instruction *val, Instruction *ptr, int64_t offset, bool isVolatile
) {
    // FIXME
    (void) isVolatile;

#ifdef JITTEFEX_ENABLE_DEBUG
    const Type &pType = ptr->getType();
    assert(pType.getBaseType() == BaseType::GCPointer ||
           pType.getBaseType() == BaseType::TaggedWord ||
           pType.getBaseType() == BaseType::GCStack);
    if (pType.getBaseType() == BaseType::GCStack)
        assert(offset == 0);
#endif

    Instruction *ret = insertionPoint->append(
        std::make_unique<GCStoreInst>(insertionPoint, val, ptr, offset));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        int stype = val->getType().getSLJITType(true);
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

        if (ptr->getType().getBaseType() == BaseType::GCStack) {
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

#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
            if (val->getType().getBaseType() == BaseType::GCPointer ||
                val->getType().getBaseType() == BaseType::TaggedWord) {
                // Both are on the stack, so move the tag too
                if (sljit_emit_op1(sc, SLJIT_MOV_U8,
                    ptr->sljitLoc.reg, ptr->sljitLoc.tag,
                    val->sljitLoc.reg, val->sljitLoc.tag)) {
                    SCANCEL();
                }
            }
#endif

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

#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
Instruction *IRBuilder::createGCLoadTag(Instruction *val) {
#ifdef JITTEFEX_ENABLE_DEBUG
    const Type &pType = val->getType();
    assert(pType.getBaseType() == BaseType::GCPointer ||
           pType.getBaseType() == BaseType::TaggedWord ||
           pType.getBaseType() == BaseType::GCStack);
#endif

    Instruction *ret = insertionPoint->append(
        std::make_unique<GCLoadTagInst>(insertionPoint, val));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();

        if (sljit_emit_op1(sc, SLJIT_MOV_U8,
            ret->sljitLoc.reg, ret->sljitLoc.off,
            val->sljitLoc.reg, val->sljitLoc.tag)) {
            SCANCEL();
        }
    }
#endif

    return ret;
}

Instruction *IRBuilder::createGCStoreTag(
    Instruction *val, Instruction *tag
) {
#ifdef JITTEFEX_ENABLE_DEBUG
    const Type &pType = val->getType();
    const Type &tType = tag->getType();
    assert(pType.getBaseType() == BaseType::GCPointer ||
           pType.getBaseType() == BaseType::TaggedWord ||
           pType.getBaseType() == BaseType::GCStack);
    assert(tType.getBaseType() == BaseType::Signed ||
           tType.getBaseType() == BaseType::Unsigned);
#endif

    Instruction *ret = insertionPoint->append(
        std::make_unique<GCStoreTagInst>(insertionPoint, val, tag));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (sljit_emit_op1(sc, SLJIT_MOV_U8,
            val->sljitLoc.reg, val->sljitLoc.tag,
            tag->sljitLoc.reg, tag->sljitLoc.off)) {
            SCANCEL();
        }
    }
#endif

    return ret;
}
#endif
#endif

// 2017
Instruction *IRBuilder::createTrunc(
    Instruction *v, const Type &destTy, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    (void) name;
    assert(v->getType().isInteger());
    return insertionPoint->append(
        std::make_unique<CastInst>(insertionPoint, Opcode::Trunc, v, destTy NAME));
}

// 2021
Instruction *IRBuilder::createZExt(
    Instruction *v, const Type &destTy, const std::string &name
) {
    (void) name;
    assert(v->getType().isInteger());
    Instruction *ret = insertionPoint->append(
        std::make_unique<CastInst>(insertionPoint, Opcode::ZExt, v, destTy NAME));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        int from = v->getType().getSLJITType();
        int to = destTy.getSLJITType();

        if (from < 0 || to < 0)
            SCANCEL();

        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();

        sljit_s32 op;
#if (defined SLJIT_64BIT_ARCHITECTURE && SLJIT_64BIT_ARCHITECTURE)
        if (from == to)
            op = SLJIT_MOV;
        else if (v->getType().getWidth() <= 4 && destTy.getWidth() == 8)
            op = SLJIT_MOV_U32;
        else
            SCANCEL();
#else
        op = SLJIT_MOV;
#endif

        if (sljit_emit_op1(sc, op, ret->sljitLoc.reg, ret->sljitLoc.off,
            v->sljitLoc.reg, v->sljitLoc.off))
            SCANCEL();
    }
#endif

    return ret;
}

// 2025
Instruction *IRBuilder::createSExt(
    Instruction *v, const Type &destTy, const std::string &name
) {
#ifdef JITTEFEX_USE_SFJIT
    abort();
#endif
    (void) name;
    assert(v->getType().isInteger());
    return insertionPoint->append(
        std::make_unique<CastInst>(insertionPoint, Opcode::SExt, v, destTy NAME));
}

// 2073
Instruction *IRBuilder::createUIToFP(
    Instruction *val, const Type &destTy, const std::string &name
) {
    (void) name;
    assert(val->getType().isInteger());
    Instruction *ret = insertionPoint->append(std::make_unique<CastInst>(
        insertionPoint, Opcode::UIToFP, val, destTy NAME));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        // FIXME: SLJIT has no unsigned versions of these!
        int fromType = val->getType().getSLJITType();
        int toType = destTy.getSLJITType();

        if (!f->sljitAllocateRegister(true, ret->sljitLoc))
            SCANCEL();

        sljit_s32 op;
        if (fromType == SLJIT_ARG_TYPE_32) {
            if (toType == SLJIT_ARG_TYPE_F32)
                op = SLJIT_CONV_F32_FROM_S32;
            else
                op = SLJIT_CONV_F64_FROM_S32;
        } else {
            if (toType == SLJIT_ARG_TYPE_F32)
                op = SLJIT_CONV_F32_FROM_SW;
            else
                op = SLJIT_CONV_F64_FROM_SW;
        }

        if (sljit_emit_fop1(sc, op,
            ret->sljitLoc.reg, ret->sljitLoc.off,
            val->sljitLoc.reg, val->sljitLoc.off))
            SCANCEL();
    }
#endif

    return ret;
}

#ifdef JITTEFEX_USE_SFJIT
// Utility function for SLJIT integer comparisons
static inline bool sicmp(
    struct sljit_compiler *sc, const Type &type, const SLJITLocation &res,
    const SLJITLocation &l, const SLJITLocation &r, sljit_s32 *flags,
    sljit_s32 *ops
) {
    sljit_s32 cmpOp = SLJIT_SUB;
    int stype = type.getSLJITType();
#if (defined SLJIT_64BIT_ARCHITECTURE && SLJIT_64BIT_ARCHITECTURE)
    if (stype == SLJIT_ARG_TYPE_32)
        cmpOp |= SLJIT_32;
#else
    if (stype == SJIT_ARG_TYPE_32)
        0; // nothing
#endif
    else if (stype != SLJIT_ARG_TYPE_W && stype != SLJIT_ARG_TYPE_P)
        return false;

    for (int i = 0; flags[i]; i++) {
        if (sljit_emit_op2u(sc, cmpOp | SLJIT_SET(flags[i]), l.reg, l.off,
            r.reg, r.off))
            return false;

        if (sljit_emit_op_flags(sc, ops[i], SLJIT_R0, 0, flags[i]))
            return false;
    }

    if (sljit_emit_op1(sc, SLJIT_MOV, res.reg, res.off, SLJIT_R0, 0))
        return false;

    return true;
}
#endif

// 2231
Instruction *IRBuilder::createICmpNE(
    Instruction *l, Instruction *r, const std::string &name
) {
    (void) name;
    assert(l->getType() == r->getType() && (
        l->getType().isInteger() ||
        l->getType().isAnyPointer()
    ));
    Instruction *ret = insertionPoint->append(
        std::make_unique<ICmpInst>(
            insertionPoint, l, r,
            false, true, true, false NAME));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        static sljit_s32 flags[] = {SLJIT_LESS, SLJIT_GREATER, 0};
        static sljit_s32 ops[] = {SLJIT_MOV, SLJIT_OR, 0};
        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();
        if (!sicmp(sc, l->getType(), ret->sljitLoc, l->sljitLoc, r->sljitLoc,
            flags, ops))
            SCANCEL();
    }
#endif

    return ret;
}

// 2259
Instruction *IRBuilder::createICmpSLT(
    Instruction *l, Instruction *r, const std::string &name
) {
    (void) name;
    assert(l->getType() == r->getType() && l->getType().isInteger());
    Instruction *ret = insertionPoint->append(
        std::make_unique<ICmpInst>(
            insertionPoint, l, r,
            true, false, true, false NAME));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        static sljit_s32 flags[] = {SLJIT_LESS, 0};
        static sljit_s32 ops[] = {SLJIT_MOV, 0};
        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();
        if (!sicmp(sc, l->getType(), ret->sljitLoc, l->sljitLoc, r->sljitLoc,
            flags, ops))
            SCANCEL();
    }
#endif

    return ret;
}

#ifdef JITTEFEX_USE_SFJIT
// Utility function for SLJIT floating point comparisons
static inline bool sfcmp(
    struct sljit_compiler *sc, const Type &type, const SLJITLocation &res,
    const SLJITLocation &l, const SLJITLocation &r, sljit_s32 *flags,
    sljit_s32 *ops
) {
    sljit_s32 cmpOp = SLJIT_CMP_F64;
    int stype = type.getSLJITType();
    if (stype == SLJIT_ARG_TYPE_F32)
        cmpOp |= SLJIT_32;
    else if (stype != SLJIT_ARG_TYPE_F64)
        return false;

    for (int i = 0; flags[i]; i++) {
        if (sljit_emit_fop1(sc, cmpOp | SLJIT_SET(flags[i]), l.reg, l.off,
            r.reg, r.off))
            return false;

        if (sljit_emit_op_flags(sc, ops[i], SLJIT_R0, 0, flags[i]))
            return false;
    }

    if (sljit_emit_op1(sc, SLJIT_MOV, res.reg, res.off, SLJIT_R0, 0))
        return false;

    return true;
}
#endif

// 2292
Instruction *IRBuilder::createFCmpONE(
    Instruction *l, Instruction *r, const std::string &name
) {
    (void) name;
    assert(l->getType() == r->getType() &&
        l->getType().getBaseType() == BaseType::Float);
    Instruction *ret = insertionPoint->append(
        std::make_unique<FCmpInst>(
            insertionPoint, l, r,
            true, true, true, false NAME));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        static sljit_s32 flags[] = {SLJIT_F_NOT_EQUAL, SLJIT_ORDERED, 0};
        static sljit_s32 ops[] = {SLJIT_MOV, SLJIT_AND, 0};
        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();
        if (!sfcmp(sc, l->getType(), ret->sljitLoc, l->sljitLoc, r->sljitLoc,
            flags, ops))
            SCANCEL();
    }
#endif

    return ret;
}

// 2322
Instruction *IRBuilder::createFCmpULT(
    Instruction *l, Instruction *r, const std::string &name
) {
    (void) name;
    assert(l->getType() == r->getType() &&
        l->getType().getBaseType() == BaseType::Float);
    Instruction *ret = insertionPoint->append(
        std::make_unique<FCmpInst>(
            insertionPoint, l, r,
            false, false, true, false NAME));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        static sljit_s32 flags[] = {SLJIT_F_LESS, SLJIT_UNORDERED, 0};
        static sljit_s32 ops[] = {SLJIT_MOV, SLJIT_OR, 0};
        if (!f->sljitAllocateRegister(false, ret->sljitLoc))
            SCANCEL();
        if (!sfcmp(sc, l->getType(), ret->sljitLoc, l->sljitLoc, r->sljitLoc,
            flags, ops))
            SCANCEL();
    }
#endif

    return ret;
}

// 2391
Instruction *IRBuilder::createCall(
    FunctionType *fTy, Instruction *callee,
    const std::vector<Instruction *> &args,
    const std::string &name
) {
    (void) name;
    assert(callee->getType().isAnyPointer());
    Instruction *ret = insertionPoint->append(
        std::make_unique<CallInst>(insertionPoint, fTy, callee, args NAME));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        struct sljit_marg *marg;
        struct sljit_alloca *alloc;
        SLJITLocation loc;
        // Location of the target code
        SLJITLocation floc;
        bool flocAllocated = false;
        // Location of the box for the function (i.e., MachineCode object)
        SLJITLocation fbox;
        bool fboxAllocated = false;
        sljit_s32 wordR, floatR, stackSpace,
            paramI, argI, wordI, floatI, stackI;
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

        // Figure out how many arguments we need
        if (sljit_marg_properties(sc, marg, &wordR, &floatR, &stackSpace))
            SCANCEL();

#ifdef JITTEFEX_ENABLE_GC_STACK
        // Including GC stack space
        sljit_sw gcStackSpace = 0;
        sljit_sw gcStackI;
        for (auto &type : fTy->getParamTypes()) {
            if (type.getBaseType() == BaseType::GCPointer ||
                type.getBaseType() == BaseType::TaggedWord) {
                gcStackSpace++;
            }
        }

#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
        // Make room for tags
        sljit_sw gcStackTagI;
        gcStackSpace += (gcStackSpace + sizeof(size_t) - 1) / sizeof(size_t);
#endif

        gcStackSpace *= sizeof(size_t);
#endif

        // Compile the function if applicable
        if (callee->getType().getBaseType() == BaseType::Pointer) {
            // The function is the return of compilation
            floc = SLJITLocation{SLJIT_S(SLJIT_NUMBER_OF_SAVED_REGISTERS), 0};
            if (wordR >= floc.reg) {
                // No use, put it on the stack
                if (!f->sljitAllocateStack(floc))
                    SCANCEL();
                flocAllocated = true;
            }

            // And we need a box for it
            if (!f->sljitAllocateStack(fbox))
                SCANCEL();
            fboxAllocated = true;

            // Get the target into the argument register
            if (sljit_emit_op1(sc, SLJIT_MOV_P, SLJIT_R0, 0,
                callee->sljitLoc.reg, callee->sljitLoc.off))
                SCANCEL();

            // Call compile
            if (sljit_emit_icall(sc, SLJIT_CALL, SLJIT_ARGS1(P, P),
                SLJIT_IMM, (sljit_sw) (void *) jittefex::compile))
                SCANCEL();

            // The return is the box
            if (sljit_emit_op1(sc, SLJIT_MOV_P, fbox.reg, fbox.off,
                SLJIT_R0, 0))
                SCANCEL();

            // Get the value out of the box
            if (sljit_emit_op1(sc, SLJIT_MOV_P, SLJIT_R0, 0,
                fbox.reg, fbox.off))
                SCANCEL();
            if (sljit_emit_op1(sc, SLJIT_MOV_P, floc.reg, floc.off,
                SLJIT_MEM1(SLJIT_R0), offsetof(MachineCode, code)))
                SCANCEL();

        } else { // CodePointer
            floc = callee->sljitLoc;
            auto it = regMap.find(floc.reg);
            if (it != regMap.end())
                floc = it->second;

        }

        // Make space
        if (stackSpace) {
            alloc = sljit_emit_alloca(sc, stackSpace);
            if (!alloc)
                SCANCEL();
        }
#ifdef JITTEFEX_ENABLE_GC_STACK
        if (gcStackSpace) {
            if (sljit_emit_op2(sc, SLJIT_SUB,
                SLJIT_GCSP, 0, SLJIT_GCSP, 0, SLJIT_IMM, gcStackSpace))
                SCANCEL();
        }
#endif

#ifdef JITTEFEX_ENABLE_GC_STACK
        // JIT stack is the first argument
        if (sljit_emit_marg_mov(sc, marg, 0, SLJIT_GCSP, 0))
            SCANCEL();
#endif

        argI = floatI = stackI = 0;
#ifdef JITTEFEX_ENABLE_GC_STACK
        paramI = wordI = 1;
#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
        gcStackI = 1;
        gcStackTagI = 0;
#else
        gcStackI = 0;
#endif
#else
        paramI = wordI = 0;
#endif
        for (auto &type : fTy->getParamTypes()) {
            sljit_s32 op;
            bool flt = false;
            loc = args[argI++]->sljitLoc;
            paramI++;

#ifdef JITTEFEX_ENABLE_GC_STACK
            if (type.getBaseType() == BaseType::GCPointer ||
                type.getBaseType() == BaseType::TaggedWord) {
                if (sljit_emit_op1(sc, SLJIT_MOV,
                    SLJIT_MEM1(SLJIT_GCSP), gcStackI * sizeof(size_t),
                    loc.reg, loc.off))
                    SCANCEL();
                gcStackI++;

#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
                if (loc.tag >= 0) {
                    // It has its own tag, so use it
                    if (sljit_emit_op1(sc, SLJIT_MOV_U8,
                        SLJIT_MEM1(SLJIT_GCSP), gcStackTagI,
                        loc.reg, loc.tag))
                        SCANCEL();
                } else if (type.getBaseType() == BaseType::GCPointer) {
                    // Use a generic GC tag
                    if (sljit_emit_op1(sc, SLJIT_MOV_U8,
                        SLJIT_MEM1(SLJIT_GCSP), gcStackTagI,
                        SLJIT_IMM, 0))
                        SCANCEL();
                } else {
                    // Use a generic tag
                    if (sljit_emit_op1(sc, SLJIT_MOV_U8,
                        SLJIT_MEM1(SLJIT_GCSP), gcStackTagI,
                        SLJIT_IMM, 1))
                        SCANCEL();
                }
                gcStackTagI++;

                // And possibly move to the next tag word
                if (gcStackTagI % sizeof(size_t) == 0) {
                    gcStackTagI = gcStackI * sizeof(size_t);
                    gcStackI++;
                }
#endif

                continue;
            }
#endif

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
                    if (sljit_emit_marg_mov(sc, marg, paramI - 1,
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
                    if (sljit_emit_marg_mov(sc, marg, paramI - 1,
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

#ifdef JITTEFEX_ENABLE_GC_TAGGED_STACK
        // Cap off the tags if necessary
        if (gcStackTagI % sizeof(size_t) != 0) {
            if (sljit_emit_op1(sc, SLJIT_MOV_U8,
                SLJIT_MEM1(SLJIT_GCSP), gcStackTagI,
                SLJIT_IMM, 0xFF))
                SCANCEL();
        }
#endif

        // Call the actual function
        if (sljit_emit_icall_multiarg(sc, marg, floc.reg, floc.off))
            SCANCEL();

        if (stackSpace && sljit_emit_pop(sc, alloc->size))
            SCANCEL();

#ifdef JITTEFEX_ENABLE_GC_STACK
        if (gcStackSpace) {
            if (sljit_emit_op2(sc, SLJIT_ADD,
                SLJIT_GCSP, 0, SLJIT_GCSP, 0, SLJIT_IMM, gcStackSpace))
                SCANCEL();
        }
#endif

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

        if (flocAllocated)
            f->sljitReleaseRegister(false, floc);

        if (fboxAllocated) {
            // FIXME: Free the code!
            f->sljitReleaseRegister(false, fbox);
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


Instruction *IRBuilder::createNop() {
    Instruction *ret = insertionPoint->append(
        std::make_unique<NopInst>(insertionPoint));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        if (sljit_emit_op0(sc, SLJIT_NOP))
            SCANCEL();
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
#ifdef JITTEFEX_ENABLE_GC_STACK
        if (ty.getBaseType() == BaseType::GCPointer ||
            ty.getBaseType() == BaseType::TaggedWord) {
            if (!f->sljitAllocateGCStack(ret->sljitLoc))
                SCANCEL();
            if (sljit_emit_op1(sc, SLJIT_MOV,
                ret->sljitLoc.reg, ret->sljitLoc.off,
                SLJIT_IMM, (sljit_sw) val))
                SCANCEL();
        } else
#endif
        ret->sljitLoc = SLJITLocation{SLJIT_IMM, (ptrdiff_t) val};
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
        ret->sljitLoc = SLJITLocation{SLJIT_IMM, (ptrdiff_t) (void *) val};
    }
#endif

    return ret;
}

Instruction *IRBuilder::createCodeLiteral(void *val) {
    Instruction *ret = insertionPoint->append(
        std::make_unique<CodeLiteralInst>(insertionPoint, val));

#ifdef JITTEFEX_USE_SFJIT
    SJ {
        ret->sljitLoc = SLJITLocation{SLJIT_IMM, (ptrdiff_t) val};
    }
#endif

    return ret;
}

}
