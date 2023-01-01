CXX=clang++
LLVM_COMPONENTS=core orcjit native
CXXFLAGS=-std=c++17 -O0 -g -DDEBUG

# With LLVM:
ALLCXXFLAGS=`llvm-config --cxxflags` -Isrc $(CXXFLAGS)
LDFLAGS=`llvm-config --ldflags`
LIBS=`llvm-config --system-libs --libs $(LLVM_COMPONENTS)`

# No LLVM:
#ALLCXXFLAGS=-Isrc $(CXXFLAGS)
#LDFLAGS=
#LIBS=

OBJS=src/builder.o src/compile.o src/ir.o src/jit.o src/sfjitLir.o src/toy.o \
     src/type.o

all: toy

toy: $(OBJS)
	$(CXX) $(ALLCXXFLAGS) $(LDFLAGS) $(OBJS) $(LIBS) -o $@

%.o: %.cc
	$(CXX) $(ALLCXXFLAGS) -c $< -o $@

src/*.cc: src/jittefex/config.h

src/jittefex/config.h: src/jittefex/config.h.in
	sed 's/@JITTEFEX_HAVE_LLVM@/#define JITTEFEX_HAVE_LLVM 1/g' < $< > $@

clean:
	rm -f $(OBJS) src/jittefex/config.h toy deps.mk

include deps.mk

deps.mk: src/jittefex/config.h src/*.cc src/jittefex/*.h
	-$(CXX) $(ALLCXXFLAGS) -MM src/*.cc > $@
