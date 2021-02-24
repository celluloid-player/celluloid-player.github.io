all: installation.html

%.html: %.html.in
	guile expand-template.scm < $< > $@

.PHONY: clean

clean:
	rm -fv installation.html
