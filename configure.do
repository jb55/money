redo-ifchange money.cabal
cabal configure || cabal install --only-dependencies && cabal configure
