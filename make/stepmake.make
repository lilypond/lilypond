# make/Stepmake.make

include $(depth)/make/toplevel-version.make

# Don't try to outsmart us, you puny computer!
ifeq (0,${MAKELEVEL})
  MAKE:=$(MAKE) --no-builtin-rules
endif
.SUFFIXES:

# Use alternate configurations alongside eachother:
#
#     ./configure --enable-configsuffix=debug
#     make conf=debug
#
# uses config-debug.make and config-debug.h; output goes to out-debug.
#
ifdef conf
  CONFIGSUFFIX=-$(conf)
endif

# Use same configuration, but different output directory:
#
#     make out=WWW
#
# uses config.make and config.h; output goes to out-WWW.
#
ifdef out
  outbase=out-$(out)
else
  outbase=out$(CONFIGSUFFIX)
endif

ifdef config
  configuration=$(config)
else
  ifeq ($(builddir),.)
    configuration=$(depth)/config$(CONFIGSUFFIX).make
  else
    # user package
    configuration=$(depth)/$(builddir)/config$(CONFIGSUFFIX).make
    # stepmake package
    #configuration=$(depth)/../$(builddir)/stepmake/config$(CONFIGSUFFIX).make
  endif
endif

include $(configuration)

ifeq ($(builddir),.)
  outroot=.
else
  outroot=$(depth)/$(builddir)/$(patsubst $(shell cd $(depth); pwd)%,%,$(shell cd .; pwd))
endif

outdir=$(outroot)/$(outbase)
config_h=$(depth)/$(builddir)/config$(CONFIGSUFFIX).h

# user package
stepdir = $(stepmake)/stepmake
# for stepmake package
# stepdir = $(depth)/stepmake

STEPMAKE_TEMPLATES := generic $(STEPMAKE_TEMPLATES) 
LOCALSTEPMAKE_TEMPLATES:= generic $(LOCALSTEPMAKE_TEMPLATES)


all:

-include $(addprefix $(depth)/make/,$(addsuffix -inclusions.make, $(LOCALSTEPMAKE_TEMPLATES)))

-include $(addprefix $(stepdir)/,$(addsuffix -inclusions.make, $(STEPMAKE_TEMPLATES)))


include $(addprefix $(stepdir)/,$(addsuffix -vars.make, $(STEPMAKE_TEMPLATES)))

# ugh. need to do this because of PATH :=$(topdir)/..:$(PATH) 
include $(addprefix $(depth)/make/,$(addsuffix -vars.make, $(LOCALSTEPMAKE_TEMPLATES))) 


include $(addprefix $(depth)/make/,$(addsuffix -rules.make, $(LOCALSTEPMAKE_TEMPLATES))) 
include $(addprefix $(stepdir)/,$(addsuffix -rules.make, $(STEPMAKE_TEMPLATES))) 
include $(addprefix $(depth)/make/,$(addsuffix -targets.make, $(LOCALSTEPMAKE_TEMPLATES))) 
include $(addprefix $(stepdir)/,$(addsuffix -targets.make, $(STEPMAKE_TEMPLATES))) 


