# -*-Makefile-*-
# title	   Yolily_Toplevel_vars.make

footify=$(PYTHON) $(step-bindir)/add-html-footer.py --name $(PACKAGE_NAME) --version $(TOPLEVEL_VERSION) \
		--footer Documentation/footer.html.in `$(FIND) . -name '*.html' -print`
