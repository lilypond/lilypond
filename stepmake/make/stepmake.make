# make/Stepmake.make

include $(depth)/make/toplevel-version.make

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

stepdir = $(depth)/stepmake

LOCALSTEPMAKE_TEMPLATES:=generic $(LOCALSTEPMAKE_TEMPLATES)
STEPMAKE_TEMPLATES := generic $(STEPMAKE_TEMPLATES) 

default:

include $(addprefix $(depth)/make/,$(addsuffix -vars.make, $(LOCALSTEPMAKE_TEMPLATES))) 
include $(addprefix $(stepdir)/,$(addsuffix -vars.make, $(STEPMAKE_TEMPLATES))) 
include $(addprefix $(depth)/make/,$(addsuffix -rules.make, $(LOCALSTEPMAKE_TEMPLATES))) 
include $(addprefix $(stepdir)/,$(addsuffix -rules.make, $(STEPMAKE_TEMPLATES))) 
include $(addprefix $(depth)/make/,$(addsuffix -targets.make, $(LOCALSTEPMAKE_TEMPLATES))) 
include $(addprefix $(stepdir)/,$(addsuffix -targets.make, $(STEPMAKE_TEMPLATES))) 



