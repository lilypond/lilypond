# title	   package specific targets

.PHONY : check-dist-deps check-make-deps check-rpm-doc-deps check-html-deps check-www-doc check-rpm-icons

check-html-deps:
	$(MAKE) -C $(depth)/Documentation html

check-www-doc:
	$(MAKE) CONFIGSUFFIX='www' -C Documentation outdirs
	$(MAKE) CONFIGSUFFIX='www' -C Documentation WWW

# check-rpm-doc-deps: 
# 	$(MAKE) -C $(depth)/Documentation gifs

check-rpm-icons:

check-make-deps:
	$(MAKE) -C $(depth)/make

