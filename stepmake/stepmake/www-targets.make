
local-WWW:

ifneq ($(strip $(depth)),.)
WWW: local-WWW
	$(LOOP)

WWW-clean: local-WWW-clean
	$(LOOP)
endif

local-web:
	$(MAKE) out=www local-WWW

web:
	$(MAKE) out=www WWW

local-WWW-clean:
	rm -f $(outdir)/*

local-web-clean:
	$(MAKE) out=www local-WWW-clean

web-clean:
	$(MAKE) out=www WWW-clean

local-help: www-targets-help

www-targets-help:
	@echo -e "\
  web         update website in out-www\n\
  web-clean   clean out-www\n\
"
