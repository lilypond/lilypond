# make/Stepmake.make

# If usescons=something then reroute to scons if user is using that.
ifdef usescons

SCONS_USER = $(wildcard $(depth)/.sconsign)
ifeq ($(SCONS_USER),)
SCONS_USER = $(wildcard $(depth)/.sconf_temp)
endif
ifneq ($(SCONS_USER),)

ifeq ($(strip $(depth)),..)
here = $(notdir $(CURDIR))
else
ifeq ($(strip $(depth)),../..)
# ZUCHT?
# here = $(notdir $(dir $(CURDIR)))/$(notdir $(CURDIR))
here = $(shell basename $$(dirname $(CURDIR)))/$(notdir $(CURDIR))
endif
endif

MAKE_TARGETS = config deb diff dist distclean doc release po		\
po-replace po-update all clean check default exe help install lib web	\
web-install web-clean TAGS

$(MAKE_TARGETS): scons

# To make this trickery complete, we could have ./configure remove
# traces of scons configuration.
scons:
	@echo "warning: $(SCONS_USER) detected, rerouting to scons"
	cd $(depth) && scons $(here) $(MAKECMDGOALS)
	false
endif
endif


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
  config_make=$(config)
else
  config_make=$(depth)/config$(CONFIGSUFFIX).make
endif

outroot=.

include $(config_make)

include $(depth)/make/toplevel-version.make

#
# suggested settings
#
# CPU_COUNT=2   ## for SMP/Multicore machine
# 
-include $(depth)/local.make

MICRO_VERSION=$(PATCH_LEVEL)
BUILD_VERSION=1


outdir=$(outroot)/$(outbase)

# why not generic ??
config_h=$(top-build-dir)/config$(CONFIGSUFFIX).hh

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

# ugh. need to do this because of PATH :=$(top-src-dir)/..:$(PATH)
include $(addprefix $(depth)/make/,$(addsuffix -vars.make, $(LOCALSTEPMAKE_TEMPLATES)))


include $(addprefix $(depth)/make/,$(addsuffix -rules.make, $(LOCALSTEPMAKE_TEMPLATES)))
include $(addprefix $(stepdir)/,$(addsuffix -rules.make, $(STEPMAKE_TEMPLATES)))
include $(addprefix $(depth)/make/,$(addsuffix -targets.make, $(LOCALSTEPMAKE_TEMPLATES)))
include $(addprefix $(stepdir)/,$(addsuffix -targets.make, $(STEPMAKE_TEMPLATES)))
