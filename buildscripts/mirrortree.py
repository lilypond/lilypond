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

def hardlink_tree (input_roots = [],
                   process_dirs = '.*',
                   strip_dir_names = '',
                   exclude_dirs = '',
                   process_files = '.*',
                   find_files = '',
                   exclude_files = '',
                   target_pattern = '',
                   targets = ['.']):
    """Mirror trees for different targets by hardlinking files.

    Arguments:
     input_roots=DIRLIST      use DIRLIST as input tree roots list
     process_dir=PATTERN      only process files in directories named PATTERN
     strip_dir_names=PATTERN  strip directories names matching PATTERN 
                                 (write their content to parent)
     exclude_dir=PATTERN      don't recurse into directories named PATTERN
     process_files=PATTERN    filters files which are hardlinked
     find_files=PATTERN       find files named PATTERN. The files list will be returned.
     exclude_files=PATTERN    exclude files named PATTERN
     target_pattern=STRING    use STRING as target root directory name pattern
     targets=DIRLIST          mkdir each directory in DIRLIST and mirrors the tree into each
    """
    process_files_re = re.compile (process_files)
    find_files_re = re.compile (find_files)
    exclude_dirs_re = re.compile (exclude_dirs)
    exclude_files_re = re.compile (exclude_files)
    process_dirs_re = re.compile (process_dirs)
    strip_dir_names_re = re.compile (strip_dir_names)
    do_strip_dir_names_re = re.compile ('/(?:' + strip_dir_names + ')')

    found_files = []

    if not '%s' in target_pattern:
        target_pattern += '%s'
    target_dirs = [target_pattern % s for s in targets]

    map (os.mkdir, target_dirs)

    for d in input_roots:
        for in_dir, dirs, files in os.walk(d):
            out_dir = strip_dir_names_re.sub ('', in_dir)
            i = 0
            while i < len(dirs):
                if exclude_dirs_re.search (dirs[i]):
                    del dirs[i]
                else:
                    if os.path.islink (os.path.join (in_dir, dirs[i])):
                        files.append (dirs[i])
                    i += 1
            if not strip_dir_names_re.match (os.path.basename (in_dir)):
                for t in target_dirs:
                    p = os.path.join (t, out_dir)
                    if not os.path.isdir (p):
                        os.mkdir (p)
            if not process_dirs_re.search (in_dir):
                continue
            for f in files:
                if exclude_files_re.match (f):
                    continue
                in_file = os.path.join (in_dir, f)
                if find_files_re.match (f):
                    found_files.append (in_file)
                if os.path.islink (in_file): # all symlinks are assumed to be relative and to point to files in the input trees
                    link_path = new_link_path (os.path.normpath (os.readlink (in_file)), in_dir, do_strip_dir_names_re)
                    for t in target_dirs:
                        os.symlink (link_path, os.path.join (t, out_dir, f))
                elif process_files_re.match (f):
                    for t in target_dirs:
                        os.link (in_file, os.path.join (t, out_dir, f))
    return found_files
