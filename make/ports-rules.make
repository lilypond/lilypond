ifeq ($(zipped),)
%.ly:
	wget $(MUTOPIA_MIRROR)/$(mutopia-dir)/$@
	$(MAKE) local-convert-ly
else
%.zip:
	wget $(MUTOPIA_MIRROR)/$(mutopia-dir)/$@

%.ly:	%-lys.zip
	unzip -n $<
	$(MAKE) local-convert-ly
endif

%/GNUmakefile:
	mkdir -p $(@D)
	$(if $(shell grep '[.]ly' $(@D)/index.html), \
		echo 'mutopia-name = $(@D)' > $@, \
		$(if $(shell grep '[.]zip' $(@D)/index.html), \
			echo -e 'zipped = true\nmutopia-name = $(@D)' > $@))
	echo -e $(GNUmakefile) >> $@


