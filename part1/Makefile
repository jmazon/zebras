all: brute brute-10 backtrack

CXXFLAGS=-O2 -std=c++17

%: %.pl
	swipl -o $@ --stand_alone=true -c $<
