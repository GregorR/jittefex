AUTOMAKE_OPTIONS=foreign

lib_LIBRARIES=libjittefex.a
bin_PROGRAMS=jittefex-config

libjittefex_a_SOURCES=\
	builder.cc compile.cc ggggc.cc ir.cc jit.cc sfjitLir.cc type.cc
libjittefex_a_CXXFLAGS=@LLVM_CXXFLAGS@ -I$(srcdir)/ggggc

jittefex_config_SOURCES=jittefex-config.cc

nobase_include_HEADERS=\
	jittefex/builder.h jittefex/compile.h jittefex/gc.h \
	jittefex/Instruction.def jittefex/instruction.h jittefex/ir.h \
	jittefex/jit.h jittefex/type.h \
	jittefex/config.h \
	jittefex/ggggc/gc-gembc.h jittefex/ggggc/gc.h \
	jittefex/ggggc/gc-portablems.h jittefex/ggggc/gen-barriers.h \
	jittefex/ggggc/push.h jittefex/ggggc/thread-locals.h \
	jittefex/ggggc/threads.h jittefex/ggggc/threads-macosx.h \
	jittefex/ggggc/threads-none.h jittefex/ggggc/threads-posix.h \
	jittefex/ggggc/threads-windows.h \
	jittefex/tl/optional.hpp
