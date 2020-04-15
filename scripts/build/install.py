import getopt
import sys
import os
import shutil
(opts, args) = getopt.getopt (sys.argv[1:], 'b:cdg:m:o:st:', [])
transform_base = None
group = None
owner = None
transform = None
mode = None
copy = False
create_dir = False

for (o,a) in opts:
    if o == '-b':
        transform_base = a
    elif o == '-c':
        copy = True
    elif o == '-d':
        create_dir = True
    elif o == '-g':
        group = a
    elif o == '-m':
        mode = int (a, base=8)
    elif o == '-o':
        owner = a
    elif o == '-s':
        strip = True
    elif o == '-t':
        transform = a
    elif o == '-h':
        print(''' Usage: $0 [OPTION]... SRCFILE DSTFILE
 or: $0 [OPTION]... SRCFILES... DIRECTORY
 or: $0 -d DIRECTORIES...

In the first form, install SRCFILE to DSTFILE, removing SRCFILE by default.
In the second, create the directory path DIR.

Options:
-b=TRANSFORMBASENAME
-c         copy source (using $cpprog) instead of moving (using $mvprog).
-d         create directories instead of installing files.
-g GROUP   $chgrp installed files to GROUP.
-m MODE    $chmod installed files to MODE.
-o USER    $chown installed files to USER.
-s         strip installed files (using $stripprog).
-t=TRANSFORM
--help     display this help and exit.
--version  display version info and exit.''')
        sys.exit (0)

if not mode:
    if create_dir:
        mode = 0o755
    else:
        mode = 0o644


chown_me = []

dest = None
if not create_dir:
    dest = args.pop()

for f in args:
    if create_dir:
        if os.path.isdir (f):
            continue

        os.makedirs (f, mode=mode)
        chown_me.append (f)
    else:
        if copy:
            if os.path.exists (dest) and not os.path.isdir (dest):
                os.remove (dest)
            shutil.copy2 (f, dest)
        else:
            shutil.move (f, dest)

        if os.path.isdir (dest):
            chown_me.append (os.path.join (dest, os.path.basename (f)))
        else:
            chown_me.append (dest)

for f in chown_me:
    os.chmod (f, mode)
    if group != None or owner != None:
        os.chown (f, group, owner)





