all: markdown.html org.html

.PHONY: clean

clean:
	-@rm -f *.html

markdown.html: example.md
	cabal run piddif -- --md $^ > $@

org.html: example.org
	cabal run piddif -- $^ > $@
