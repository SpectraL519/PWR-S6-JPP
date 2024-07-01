SOURCES = binomial.hs mergesort.hs diophantine.hs prime_factors.hs totient.hs primes.hs
EXECUTABLES = $(SOURCES:.hs=)

all: clean build

build: $(EXECUTABLES)

%: %.hs
	ghc $<

clean:
	rm -f $(EXECUTABLES) *.o *.hi

.PHONY: all build clean
