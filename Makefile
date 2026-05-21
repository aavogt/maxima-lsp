
all: hoverdb/hoverdb.lmdb
	cabal install

.PHONY: all
.ONESHELL:

hoverdb/hoverdb.lmdb: deps/maxima_singlepage.html hoverdb/hoverdb.hs
	cd hoverdb
	cabal run

deps/maxima_singlepage.html:
	curl https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html -o deps/maxima_singlepage
