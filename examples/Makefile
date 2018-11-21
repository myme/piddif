all: markdown.html org.html

.PHONY: clean

clean:
	-@rm -f *.html

markdown.html: example.md
	stack build --exec "piddif --md $^" > $@

org.html: example.org
	stack build --exec "piddif $^" > $@
