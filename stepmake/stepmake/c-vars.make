include $(stepdir)/compile-vars.make

H_FILES := $(call src-wildcard,*.h)
C_FILES := $(call src-wildcard,*.c)
Y_FILES := $(call src-wildcard,*.y)
L_FILES := $(call src-wildcard,*.l)

SOURCE_FILES+=$(Y_FILES) $(C_FILES) $(L_FILES) $(H_FILES)

O_FILES+=$(addprefix $(outdir)/, $(Y_FILES:.y=.o) $(C_FILES:.c=.o) $(L_FILES:.l=.o))

TAGS_SOURCES += $(C_FILES)
TAGS_HEADERS += $(H_FILES)

ALL_C_SOURCES += $(H_FILES) $(C_FILES) $(Y_FILES) $(L_FILES)

ALL_CPPFLAGS = $(CPPFLAGS) $(CONFIG_CPPFLAGS) $(DEFINES) $(INCLUDES:%=-I%)
ALL_CFLAGS = $(CFLAGS) $(ALL_CPPFLAGS) $(CONFIG_CFLAGS) $(MODULE_CFLAGS) $(EXTRA_CFLAGS)
