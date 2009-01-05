#!@PYTHON@

import re
import os

def new_link_path (link, dir, r):
    l = link.split ('/')
    d = dir.split ('/')
    i = 0
    while i < len(d) and i < len(l) and l[i] == '..':
        if r.match (d[i]):
            del l[i]
        else:
            i += 1
    return '/'.join ([x for x in l if not r.match (x)])

def walk_tree (tree_roots = [],
               process_dirs = '.*',
               exclude_dirs = '',
               find_files = '.*',
               exclude_files = ''):
    """Walk directory trees and.returns (dirs, symlinks, files, extra_files) tuple.

    Arguments:
     tree_roots=DIRLIST      use DIRLIST as tree roots list
     process_dir=PATTERN      only process files in directories named PATTERN
     exclude_dir=PATTERN      don't recurse into directories named PATTERN
     find_files=PATTERN    filters files which are hardlinked
     exclude_files=PATTERN    exclude files named PATTERN
    """
    find_files_re = re.compile (find_files)
    exclude_dirs_re = re.compile (exclude_dirs)
    exclude_files_re = re.compile (exclude_files)
    process_dirs_re = re.compile (process_dirs)

    dirs_paths = []
    symlinks_paths = []
    files_paths = []

    for d in tree_roots:
        for current_dir, dirs, files in os.walk(d):
            i = 0
            while i < len(dirs):
                if exclude_dirs_re.search (os.path.join (current_dir, dirs[i])):
                    del dirs[i]
                else:
                    p = os.path.join (current_dir, dirs[i])
                    if os.path.islink (p):
                        symlinks_paths.append (p)
                    i += 1
            if not process_dirs_re.search (current_dir):
                continue
            dirs_paths.append (current_dir)
            for f in files:
                if exclude_files_re.match (f):
                    continue
                p = os.path.join (current_dir, f)
                if os.path.islink (p):
                    symlinks_paths.append (p)
                elif find_files_re.match (f):
                    files_paths.append (p)
    return (dirs_paths, symlinks_paths, files_paths)
