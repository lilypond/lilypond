AT_FILES = $(BLURBS) # 
at-dir = $(doc-dir)/
at-ext = .in

footify=$(PYTHON) $(step-bindir)/add-html-footer.py --index=../$(depth)/ --name $(PACKAGE_NAME) --version $(TOPLEVEL_VERSION) --header=$(depth)/Documentation/header.html.in --footer $(depth)/Documentation/footer.html.in
deep-footify=$(PYTHON) $(step-bindir)/add-html-footer.py --index=../../$(depth)/ --name $(PACKAGE_NAME) --version $(TOPLEVEL_VERSION) --header=$(depth)/Documentation/header.html.in --footer $(depth)/Documentation/footer.html.in

footify-all-command=$(footify) `$(FIND) . -name '*.html' -print`

