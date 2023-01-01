AUTOMAKE_OPTIONS=foreign

lib_LIBRARIES=libjittefex.a

libjittefex_a_SOURCES=\
	builder.cc compile.cc ir.cc jit.cc sfjitLir.cc type.cc
libjittefex_a_CXXFLAGS=@LLVM_CXXFLAGS@ -std=c++17

nobase_include_HEADERS=\
	jittefex/builder.h jittefex/compile.h jittefex/Instruction.def \
	jittefex/instruction.h jittefex/ir.h jittefex/jit.h jittefex/type.h \
	jittefex/config.h
