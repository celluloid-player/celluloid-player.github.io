PAGES=installation.html bug-reports.html faq.html

all: $(PAGES)

%.html: %.html.in
	guile expand-template.scm < $< > $@

.PHONY: clean

clean:
	rm -fv $(PAGES)
