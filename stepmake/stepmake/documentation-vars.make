# Documentation files
AT_FILES = $(BLURBS) # 
at-dir = $(doc-dir)/
at-ext = .in

footify=$(PYTHON) $(step-bindir)/add-html-footer.py --name $(PACKAGE_NAME) --version $(TOPLEVEL_VERSION) --footer $(depth)/Documentation/footer.html.in

footify-all-command=$(footify) `$(FIND) . -name '*.html' -print`

