# title	   package specific targets
# file	   make/Targets.make


# urg!
$(configheader): $(depth)/$(configuration).hh
	cp $< $@

