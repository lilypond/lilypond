# mutopia/Makefile

depth = ../..

# do Coriolan if you have plenty RAM / CPU / quotum
# examples=coriolan

LOCALSTEPMAKE_TEMPLATES=mutopia

include $(depth)/make/stepmake.make

parts=$(patsubst %.ly,%,$(wildcard *-part.ly))

tarball=coriolan
mutopia-examples=coriolan $(parts)



