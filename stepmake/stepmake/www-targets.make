web:
	$(MAKE) out=www WWW

local-help: www-targets-help

www-targets-help:
	@echo -e "\
  web         update website in out-www\n\
  web-clean   clean out-www\n\
"
