#!@PYTHON@

## This is www_post.py. This script is the main stage
## of toplevel GNUmakefile local-WWW-post target.

# USAGE: www_post PACKAGE_NAME TOPLEVEL_VERSION OUTDIR TARGETS
# please call me from top of the source directory

import sys
import os
import re

import langdefs

import mirrortree
import postprocess_html

package_name, package_version, outdir, targets = sys.argv[1:]
targets = targets.split (' ')
outdir = os.path.normpath (outdir)
doc_dirs = ['input', 'Documentation', outdir]
target_pattern = os.path.join (outdir, '%s-root')

# these redirection pages allow to go back to the documentation index
# from HTML manuals/snippets page
static_files = {
    os.path.join (outdir, 'index.html'):
        '''<META HTTP-EQUIV="refresh" content="0;URL=Documentation/index.html">
<html><body>Redirecting to the documentation index...</body></html>\n''',
    os.path.join (outdir, 'VERSION'):
        package_version + '\n',
    }

for f, contents in static_files.items ():
    open (f, 'w').write (contents)

sys.stderr.write ("Mirroring...\n")
dirs, symlinks, files = mirrortree.walk_tree (
    tree_roots = doc_dirs,
    process_dirs = outdir,
    exclude_dirs = '(^|/)((' + \
        r'po|xref-maps|out|out-test|out-cov|.*?[.]t2d|\w*?-root)|^Documentation/(' + \
        '|'.join ([l.code for l in langdefs.LANGUAGES]) + '))(/|$)',
    find_files = r'.*?\.(?:midi|html|pdf|png|txt|i?ly|signature|css|zip)$|VERSION',
    exclude_files = r'lily-[0-9a-f]+.*\.(pdf|txt)')

# actual mirrorring stuff
html_files = []
hardlinked_files = []
for f in files:
    if f.endswith ('.html'):
        html_files.append (f)
    else:
        hardlinked_files.append (f)
dirs = [re.sub ('/' + outdir, '', d) for d in dirs]
while outdir in dirs:
    dirs.remove (outdir)
dirs = list (set (dirs))
dirs.sort ()

strip_file_name = {}
strip_re = re.compile (outdir + '/')
for t in targets:
    out_root = target_pattern % t
    strip_file_name[t] = lambda s: os.path.join (target_pattern % t, (strip_re.sub ('', s)))
    os.mkdir (out_root)
    map (os.mkdir, [os.path.join (out_root, d) for d in dirs])
    for f in hardlinked_files:
        os.link (f, strip_file_name[t] (f))
    for l in symlinks:
        p = mirrortree.new_link_path (os.path.normpath (os.readlink (l)), os.path.dirname (l), strip_re)
        dest = strip_file_name[t] (l)
        if not os.path.exists (dest):
            os.symlink (p, dest)


# need this for content negotiation with documentation index
if 'online' in targets:
    f = open (os.path.join (target_pattern % 'online', 'Documentation/.htaccess'), 'w')
    f.write ('#.htaccess\nDirectoryIndex index\n')
    f.close ()

postprocess_html.build_pages_dict (html_files)
for t in targets:
    sys.stderr.write ("Processing HTML pages for %s target...\n" % t)
    postprocess_html.process_html_files (
        package_name = package_name,
        package_version = package_version,
        target = t,
        name_filter = strip_file_name[t])

