include $(stepdir)/compile-vars.make

H_FILES := $(wildcard *.h)
C_FILES := $(wildcard *.c)
Y_FILES := $(wildcard *.y)
L_FILES := $(wildcard *.l)

SOURCE_FILES+=$(Y_FILES) $(C_FILES) $(L_FILES) $(H_FILES)

O_FILES+=$(addprefix $(outdir)/, $(Y_FILES:.y=.o) $(C_FILES:.c=.o) $(L_FILES:.l=.o))

TAGS_FILES += $(C_FILES) $(H_FILES)

ALL_C_SOURCES += $(H_FILES) $(C_FILES) $(Y_FILES) $(L_FILES)

ALL_CFLAGS = $(CFLAGS) $(ICFLAGS) $(DEFINES) $(addprefix -I,$(INCLUDES)) $(USER_CFLAGS) $(EXTRA_CFLAGS) $(MODULE_CFLAGS)
