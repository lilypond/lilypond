
# header files:
H_FILES := $(wildcard *.h)
C_FILES := $(wildcard *.c)
Y_FILES := $(wildcard *.y)
L_FILES := $(wildcard *.l)

SOURCE_FILES+=$(Y_FILES) $(C_FILES) $(L_FILES) $(H_FILES)
OBJECT_FILES+=$(Y_FILES:.y=.o) $(C_FILES:.c=.o) $(L_FILES:.l=.o)

TAGS_FILES += C_FILES H_FILES
# C/C++
# 

ALL_C_SOURCES += $(H_FILES) $(C_FILES) $(Y_FILES)

# compiler:
#
DO_C_COMPILE = $(DODEP) $(CC) -c $(CFLAGS) $(C_OUTPUT_OPTION) 
C_OUTPUT_OPTION = $< -o $@

CFLAGS = $(ICFLAGS) $(DEFINES) $(INCLUDES) $(USER_CFLAGS) $(EXTRA_CFLAGS) $(MODULE_CFLAGS)
