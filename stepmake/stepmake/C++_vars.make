
# added two warnings that are treated by cygwin32's gcc 2.7.2 as errors.
# huh, but still, no warnings even provoced with linux's gcc 2.7.2.1?

# -pipe makes it go faster, but is not supported on all platforms. 
# EXTRA_CXXFLAGS= -Wall -Winline -W -Wmissing-prototypes -Wmissing-declarations -Wconversion
EXTRA_CXXFLAGS= -Wall -W -Wmissing-prototypes -Wmissing-declarations -Wconversion


CXXFLAGS = $(ICFLAGS) $(DEFINES) $(INCLUDES) $(USER_CFLAGS) $(EXTRA_CFLAGS) $(MODULE_CFLAGS) $(USER_CXXFLAGS) $(EXTRA_CXXFLAGS) $(MODULE_CXXFLAGS)
CXX_OUTPUT_OPTION = $< -o $@
DO_CXX_COMPILE=$(DODEP) $(CXX) -c $(CXXFLAGS) $(CXX_OUTPUT_OPTION)


