CXX=clang++
LLVM_COMPONENTS=core orcjit native
CXXFLAGS=-std=c++17 -O0 -g -DDEBUG

# With LLVM:
ALLCXXFLAGS=`llvm-config --cxxflags` $(CXXFLAGS)
LDFLAGS=`llvm-config --ldflags`
LIBS=`llvm-config --system-libs --libs $(LLVM_COMPONENTS)`

# No LLVM:
#ALLCXXFLAGS=$(CXXFLAGS)
#LDFLAGS=
#LIBS=

OBJS=builder.o compile.o ir.o jit.o sfjitLir.o toy.o type.o

all: toy

toy: $(OBJS)
	$(CXX) $(ALLCXXFLAGS) $(LDFLAGS) $(OBJS) $(LIBS) -o $@

%.o: %.cc
	$(CXX) $(ALLCXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) toy deps.mk

include deps.mk

deps.mk: *.cc */*.h
	-$(CXX) $(ALLCXXFLAGS) -MM *.cc > $@
