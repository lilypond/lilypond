
# -> mutopia-vars.make
MUTOPIA_MIRROR = http://www.mutopiaproject.org/ftp
# ugh: doesn't work
# mutopia-dir = $(pwd:%/mutopia/%=mutopia)
mutopia-dir = $(shell pwd | sed 's@.*mutopia@@')


wget-list = $(mutopia-examples:%=$(mutopia-dir)/%)

dirs-ignore = GNUmakefile out out-% index.html ftp
dirs-before = $(SUBDIRS)
dirs-after = $(filter-out .,$(shell find . -maxdepth 1 -type d -not -name 'out*'))


GNUmakefile = '\
depth = $(depth)/..\n\
include $$(depth)/ports/ports.make\n\
'


