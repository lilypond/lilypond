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

stepdir = $(depth)/$(stepmake)/stepmake
-include $(stepdir)/Include.make

