# make/Stepmake.make

include $(depth)/make/Toplevel_version.make

# Don't try to outsmart us, you puny computer!
ifeq (0,${MAKELEVEL})
  MAKE:=$(MAKE) --no-builtin-rules
endif

ifndef config
  configuration=config
else
  configuration=config-$(config)
endif

include $(depth)/$(configuration).make

ifdef CONFIGSUFFIX
outdir=out-$(CONFIGSUFFIX)
else
outdir=out
endif



stepdir = $(depth)/$(stepmake)/stepmake
include $(stepdir)/Include.make

MAKEFILES := Generic $(MAKEFILES) 

include $(addprefix $(stepdir)/,$(addsuffix _vars.make, $(MAKEFILES))) 
include $(addprefix $(depth)/make/,$(addsuffix _vars.make, $(LOCALMAKEFILES))) 
include $(addprefix $(stepdir)/,$(addsuffix _rules.make, $(MAKEFILES))) 
include $(addprefix $(depth)/make/,$(addsuffix _rules.make, $(LOCALMAKEFILES))) 
include $(addprefix $(stepdir)/,$(addsuffix _targets.make, $(MAKEFILES))) 
include $(addprefix $(depth)/make/,$(addsuffix _targets.make, $(LOCALMAKEFILES))) 

# ugh.  ugh ugh ugh
$(depth)/$(configuration).make: $(depth)/configure
	@echo "**************************************"
	@echo "configure changed! You should probably reconfigure manually."
	@echo "**************************************"
	(cd $(depth); ./config.status)


