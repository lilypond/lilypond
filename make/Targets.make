# title	   package specific targets
# file	   make/Targets.make


check-dist-deps: 
	false

check-html-deps:
	false
	$(MAKE) -C $(depth)/Documentation html


check-mf-deps:
	false
	$(MAKE) -C $(depth)/mf

check-texinfo-deps:
	false
	$(MAKE) -C $(depth)/Documentation texinfo
	$(MAKE) -C $(depth)/Documentation/man texinfo

# urg!
$(configheader): $(depth)/$(configuration).hh
	cp $< $@

