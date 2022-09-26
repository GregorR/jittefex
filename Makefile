CXX=clang++
LLVM_COMPONENTS=core orcjit native
CXXFLAGS=-O0 -g
ALLCXXFLAGS=`llvm-config --cxxflags` $(CXXFLAGS)
LDFLAGS=`llvm-config --ldflags`
LIBS=`llvm-config --system-libs --libs $(LLVM_COMPONENTS)`

OBJS=jittefex.o toy.o

all: toy

toy: $(OBJS)
	$(CXX) $(ALLCXXFLAGS) $(LDFLAGS) $(OBJS) $(LIBS) -o $@

%.o: %.cc
	$(CXX) $(ALLCXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) toy deps.mk

include deps.mk

deps.mk:
	-$(CXX) $(ALLCXXFLAGS) -MM *.cc > $@
