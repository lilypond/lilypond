# make/Stepmake.make

include $(depth)/make/toplevel-version.make

# Use alternate configurations alongside eachother:
#
#     ./configure --enable-config=debug
#     make conf=debug
#
# uses config-debug.make and config-debug.h; output goes to out-debug.
#
ifdef conf
  CONFIGSUFFIX=-$(conf)
endif

# Use same configuration, but different output directory:
#
#     make out=www
#
# uses config.make and config.h; output goes to out-www.
#
ifdef out
  outbase=out-$(out)
else
  outbase=out$(CONFIGSUFFIX)
endif

ifdef config
  configuration=$(config)
else
  ifeq ($(builddir),)
    configuration=$(depth)/config$(CONFIGSUFFIX).make
  else
    configuration=$(builddir)/config$(CONFIGSUFFIX).make
  endif
endif

ifeq ($(builddir),)
  outroot=.
else
  outroot=$(builddir)/$(patsubst $(shell cd $(depth); pwd)%,%,$(pwd))
endif

include $(configuration)

outdir=$(outroot)/$(outbase)
config_h=$(builddir)/config$(CONFIGSUFFIX).h

# The outdir that was configured for: best guess to find binaries
outconfbase=out$(CONFIGSUFFIX)
outconfdir=$(outroot)/$(outconfbase)

# user package
stepdir = $(stepmake)/stepmake
# for stepmake package
# stepdir = $(depth)/stepmake

STEPMAKE_TEMPLATES := generic $(STEPMAKE_TEMPLATES) 
LOCALSTEPMAKE_TEMPLATES:= generic $(LOCALSTEPMAKE_TEMPLATES)

# Don't try to outsmart us, you puny computer!
# Well, UGH.  This only removes builtin rules from
# subsequent $(MAKE)s, *not* from the current run!
ifeq ($(BUILTINS_REMOVED),)
  export BUILTINS_REMOVED = yes
  MAKE:=$(MAKE) --no-builtin-rules
  include $(stepdir)/no-builtin-rules.make
endif
.SUFFIXES:

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


