import re
import sys

PROGRAM = sys.argv[0]
VERSION = sys.argv[1]
defs = []
for i in open (VERSION).readlines ():
    i = re.sub ('#.*','', i)
    m  = re.search ('([^ =]*)[\t ]*=[ \t]*([^ \t]*)[ \t]*\n', i)
    if m:
        defs.append ((m.group (1), m.group (2)))

sys.stdout.write (r'''
/*
 Automatically generated from %(VERSION)s
 by %(PROGRAM)s.
*/
#ifndef VERSION_HH
#define VERSION_HH
''' % vars ())

for name, expansion in defs:
    # GUILE leaks autoconf data into userspace.
    sys.stdout.write (r'''
#ifdef %(name)s
#undef %(name)s
#endif /* %(name)s */
#define %(name)s "%(expansion)s"
''' % vars ())

if ('MY_PATCH_LEVEL', '') in defs:
    sys.stdout.write (r'''
#define NO_MY_PATCHLEVEL
#define TOPLEVEL_VERSION MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL
''')
else:
    sys.stdout.write (r'''
#define TOPLEVEL_VERSION MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL "." MY_PATCH_LEVEL
''')

sys.stdout.write(r'''
#endif /* VERSION_HH */
''')

