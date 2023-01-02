#ifndef JITTEFEX_TYPE_H
#define JITTEFEX_TYPE_H 1

#include "jittefex/config.h"

#ifdef JITTEFEX_HAVE_LLVM
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#endif

#include <vector>

namespace jittefex {

/**
 * Basic types are one of five categories: void, signed, unsigned, floating
 * point, or pointer. Everything else is a refinement of one of those.
 */
enum BaseType {
    Void,
    Signed,
    Unsigned,
    Float,
    Pointer
};

/**
 * *Every* type is of this class (excluding function signature types), with the
 * elements filled in differently. *This class is intended to be passed by
 * copy.*
 */
class Type {
    private:
        BaseType baseType;

        // For numbers
        unsigned short width;

        // Reserved
        unsigned short reserved;

        Type(BaseType baseType, unsigned short width)
            : baseType{baseType}, width{width}
            {}

    public:
        // Constructors:
        static inline Type voidType() {
            return Type{BaseType::Void, 0};
        }

        static inline Type signedType(unsigned short width) {
            return Type{BaseType::Signed, width};
        }

        static inline Type signedWordType() {
            return Type{BaseType::Signed, sizeof(void *)};
        }

        static inline Type unsignedType(unsigned short width) {
            return Type{BaseType::Unsigned, width};
        }

        static inline Type unsignedWordType() {
            return Type{BaseType::Unsigned, sizeof(void *)};
        }

        static inline Type floatType(unsigned short width) {
            return Type{BaseType::Float, width};
        }

        static inline Type pointerType() {
            return Type{BaseType::Pointer, sizeof(void *)};
        }

        /**
         * So that types can be map keys.
         */
        inline bool operator<(const Type &other) const {
            (void) reserved;
            if (baseType < other.baseType)
                return true;
            if (baseType > other.baseType)
                return false;
            if (width < other.width)
                return true;
            else
                return false;
        }

        /**
         * To compare types.
         */
        inline bool operator==(const Type &other) const {
            return (baseType == other.baseType && width == other.width);
        }

#ifdef JITTEFEX_HAVE_LLVM
        // Convert to an LLVM type
        llvm::Type *getLLVMType(llvm::LLVMContext &context) const;
#endif

#ifdef JITTEFEX_HAVE_SFJIT
	// Convert to an sljit type, if possible
	int getSLJITType() const;
#endif

        inline const BaseType getBaseType() const { return baseType; }
        inline const short getWidth() const { return width; }
};

/**
 * Function types are really signatures.
 */
class FunctionType {
    private:
        Type returnType;
        std::vector<Type> paramTypes;
        bool varArg; // Not supported

        FunctionType(
            const Type &returnType, const std::vector<Type> &paramTypes, bool isVarArg
        )
            : returnType{returnType}
            , paramTypes{paramTypes}
            , varArg{isVarArg}
            {}

    public:
        /**
         * Only this getter is allowed for getting function types, to avoid
         * duplication.
         */
        static FunctionType *get(
            const Type &returnType, const std::vector<Type> &paramTypes, bool isVarArg
        );

#ifdef JITTEFEX_HAVE_LLVM
        // Convert to an LLVM function type
        llvm::FunctionType *getLLVMFunctionType(
            llvm::LLVMContext &context
        ) const;
#endif

#ifdef JITTEFEX_HAVE_SFJIT
	/* Convert to an sljit type, if possible. Actually
         * struct sljit_compiler * -> struct sljit_marg *. */
        void *getSLJITType(void *scvp) const;
#endif

        const Type &getReturnType() const { return returnType; }
        Type getReturnType() { return returnType; }
        const std::vector<Type> &getParamTypes() const { return paramTypes; }
        bool getVarArg() { return varArg; }
};

}

#endif
