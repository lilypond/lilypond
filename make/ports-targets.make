
local-ly-clean:
	$(if $(wildcard *.ly), -mv -f $(wildcard *.ly) $(outdir))

ly-clean: local-ly-clean
	$(LOOP)

local-download: $(mutopia-examples:%=%.ly)
	@echo downloading $<

download: local-download
	$(LOOP)

#
# Lots smarter would be to do one recursive wget, getting all index.html,
# parse those with python script to setup tree.
#
local-sync:
	wget --recursive --no-parent --level=1 --timestamping --dont-remove-listing --no-host-directories --accept=index.html $(MUTOPIA_MIRROR)/$(mutopia-dir)/
# ugh
# -mv ./ftp/$(mutopia-dir)/* .
	cp -r ./ftp/$(mutopia-dir)/* .
	rm -rf ./ftp
	$(MAKE) 'dirs-before=$(dirs-before)' generate-GNUmakefiles

#
# should we dowload .ly and .zip just to fill-in tree?
#
sync: local-sync
#	wget --recursive --no-parent --timestamping --dont-remove-listing --no-host-directories --cut-dirs=2 --accept=foo $(MUTOPIA_MIRROR)/$(mutopia-dir)/
	$(LOOP)

generate-GNUmakefiles: $(dirs-after:%=%/GNUmakefile)

# too time-consuming?
# local-dist: local-ly-clean

local-help: local-ports-help

local-ports-help:
	@echo -e "\
  download    download .lys from $(MUTOPIA_MIRROR)\n\
  ly-clean    move all .lys to $(outdir)\n\
  sync        generate missing parts of tree\n\
"\

