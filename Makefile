all:
	cabal install minisat-solver==0.1
	cabal v2-configure 
	cabal v2-build
	cp dist-newstyle/build/x86_64-linux/ghc-8.6.4/satFactorization-0.1.0.0/x/satFactorization-exe/build/satFactorization-exe/satFactorization-exe ./