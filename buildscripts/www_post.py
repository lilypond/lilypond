#!@PYTHON@

## This is www_post.py. This script is the main stage
## of toplevel GNUmakefile local-WWW-post target.

# USAGE: www_post PACKAGE_NAME TOPLEVEL_VERSION BUILDSCRIPT-DIR OUTDIR TARGETS
# please call me from top of the source directory

import sys
import os
import re

package_name, package_version, buildscript_dir, outdir, targets = sys.argv[1:]
targets = targets.split (' ')
outdir = os.path.normpath (outdir)
doc_dirs = ['input', 'Documentation', outdir]
target_pattern = os.path.join (outdir, '%s-root')

static_files = {os.path.join (outdir, 'index.html'):
               '''<META HTTP-EQUIV="refresh" content="0;URL=Documentation/index.html">
<html><body>Redirecting to the documentation index...</body></html>\n''',
               os.path.join (outdir, 'VERSION'):
               package_version + '\n' }

for f in static_files.keys():
    open (f, 'w').write (static_files[f])


sys.path.append (buildscript_dir)
import mirrortree
import add_html_footer
import langdefs

sys.stderr.write ("Mirrorring...\n")
html_list = mirrortree.hardlink_tree (input_roots = doc_dirs,
                          process_dirs = outdir,
                          strip_dir_names = outdir,
                          exclude_dirs = '(' +
                                      '|'.join ([l.code for l in langdefs.LANGUAGES]) +
                                      r'|po|out|\w*?-root)(/|$)',
                          process_files = r'.*?\.(?:midi|pdf|png|txt|ly|signature)$|VERSION',
                          exclude_files = r'lily-[0-9a-f]+.*\.pdf',
                          target_pattern = target_pattern,
                          targets = targets)
html_dict = add_html_footer.build_pages_dict (html_list)
strip_re = re.compile (outdir + '/')
for t in targets:
    sys.stderr.write ("Processing HTML pages for %s target...\n" % t)
    add_html_footer.add_html_footer (
        package_name = package_name,
        package_version = package_version,
        target = t,
        mail_address = 'http://post.gmane.org/post.php?group=gmane.comp.gnu.lilypond.bugs',
        pages_dict = html_dict,
        out_root = target_pattern % t,
        name_filter = lambda s: strip_re.sub ('', s))
