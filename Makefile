CXX=clang++
LLVM_COMPONENTS=core orcjit native
CXXFLAGS=-O3 -g
ALLCXXFLAGS=`llvm-config --cxxflags` $(CXXFLAGS)
LDFLAGS=`llvm-config --ldflags`
LIBS=`llvm-config --system-libs --libs $(LLVM_COMPONENTS)`

OBJS=toy.o

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
