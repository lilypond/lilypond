# title	   makefile for stepmake

# subdir level:
#
depth = ..
#

# descent order into subdirectories:
#
SUBDIRS =
#

MAKEFILES=Makedir


# bootstrap stepmake:
#
-include $(depth)/make/Stepmake.make 

# gory hack: stepmake itself is a stepmake package.
default:
	rm -f stepmake
	ln -s ../stepmake .
	rm -f bin
	ln -s ../bin .
