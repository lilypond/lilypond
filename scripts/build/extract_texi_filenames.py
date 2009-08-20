#!@PYTHON@
# -*- coding: utf-8 -*-
# extract_texi_filenames.py

# USAGE:  extract_texi_filenames.py [-o OUTDIR] FILES
#
# -o OUTDIR specifies that output files should rather be written in OUTDIR
#
# Description:
# This script parses the .texi file given and creates a file with the
# nodename <=> filename/anchor map.
# The idea behind: Unnumbered subsections go into the same file as the
# previous numbered section, @translationof gives the original node name,
# which is then used for the filename/anchor.
#
# If this script is run on a file texifile.texi, it produces a file
# texifile[.LANG].xref-map with tab-separated entries of the form
#        NODE\tFILENAME\tANCHOR
# LANG is the document language in case it's not 'en'
# Note: The filename does not have any extension appended!
# This file can then be used by our texi2html init script to determine
# the correct file name and anchor for external refs

import sys
import re
import os
import getopt

options_list, files = getopt.getopt (sys.argv[1:],'o:s:hI:m:',
                                     ['output=', 'split=',
                                      'help', 'include=',
                                      'master-map-file='])

help_text = r"""Usage: %(program_name)s [OPTIONS]... TEXIFILE...
Extract files names for texinfo (sub)sections from the texinfo files.

Options:
 -h, --help                     print this help
 -I, --include=DIRECTORY        append DIRECTORY to include search path
 -m, --master-map-file=FILE     use FILE as master map file
 -o, --output=DIRECTORY         write .xref-map files to DIRECTORY
 -s, --split=MODE               split manual according to MODE. Possible values
                                are section and custom (default)
"""

def help (text):
    sys.stdout.write ( text)
    sys.exit (0)

outdir = '.'
split = "custom"
include_path = ['.',]
master_map_file = ''
initial_map = {}
for opt in options_list:
    o = opt[0]
    a = opt[1]
    if o == '-h' or o == '--help':
        help (help_text % vars ())
    if o == '-I' or o == '--include':
        if os.path.isdir (a):
            include_path.append (a)
        else:
            print 'NOT A DIR from: ', os.getcwd (), a
    elif o == '-o' or o == '--output':
        outdir = a
    elif o == '-s' or o == '--split':
        split = a
    elif o == '-m' or o == '--master-map-file':
        if os.path.isfile (a):
            master_map_file = a
    else:
        raise Exception ('unknown option: ' + o)


if not os.path.isdir (outdir):
    if os.path.exists (outdir):
        os.unlink (outdir)
    os.makedirs (outdir)

include_re = re.compile (r'@include ((?!../lily-).*?\.i?texi)$', re.M)
whitespaces = re.compile (r'\s+')
section_translation_re = re.compile ('^@(node|(?:unnumbered|appendix)\
(?:(?:sub){0,2}sec)?|top|chapter|(?:sub){0,2}section|\
(?:major|chap|(?:sub){0,2})heading|lydoctitle|translationof) \
(.+)$', re.MULTILINE)
external_node_re = re.compile (r'\s+@c\s+external.*')

def expand_includes (m, filename):
    include_name = m.group (1)
    filepath = os.path.join (os.path.dirname (filename), include_name)
    if os.path.exists (filepath):
        return extract_sections (filepath)[1]
    else:
        for directory in include_path:
            filepath = os.path.join (directory, include_name)
            if os.path.exists (filepath):
                return extract_sections (filepath)[1]
        print 'No such file: ' + include_name
        print 'Search path: ' + ':'.join (include_path)
        return ''

lang_re = re.compile (r'^@documentlanguage (.+)', re.M)

def extract_sections (filename):
    result = ''
    f = open (filename, 'r')
    page = f.read ()
    f.close()
    # Search document language
    m = lang_re.search (page)
    if m and m.group (1) != 'en':
        lang_suffix = '.' + m.group (1)
    else:
        lang_suffix = ''
    # Replace all includes by their list of sections and extract all sections
    page = include_re.sub (lambda m: expand_includes (m, filename), page)
    sections = section_translation_re.findall (page)
    for sec in sections:
        result += "@" + sec[0] + " " + sec[1] + "\n"
    return (lang_suffix, result)

# Convert a given node name to its proper file name (normalization as
# explained in the texinfo manual:
# http://www.gnu.org/software/texinfo/manual/texinfo/html_node/HTML-Xref-Node-Name-Expansion.html
def texinfo_file_name(title):
    # exception: The top node is always mapped to index.html
    if title == "Top":
        return "index"
    # File name normalization by texinfo (described in the texinfo manual):
    # 1/2: letters and numbers are left unchanged
    # 3/4: multiple, leading and trailing whitespace is removed
    title = title.strip ();
    title = whitespaces.sub (' ', title)
    # 5:   all remaining spaces are converted to '-'
    # 6:   all other 7- or 8-bit chars are replaced by _xxxx (xxxx=ascii character code)
    result = ''
    for index in range(len(title)):
        char = title[index]
        if char == ' ': # space -> '-'
            result += '-'
        elif ( ('0' <= char and char <= '9' ) or
               ('A' <= char and char <= 'Z' ) or
               ('a' <= char and char <= 'z' ) ):  # number or letter
            result += char
        else:
            ccode = ord(char)
            if ccode <= 0xFFFF:
                result += "_%04x" % ccode
            else:
                result += "__%06x" % ccode
    # 7: if name begins with number, prepend 't_g' (so it starts with a letter)
    if (result != '') and (ord(result[0]) in range (ord('0'), ord('9'))):
        result = 't_g' + result
    return result

texinfo_re = re.compile (r'@.*{(.*)}')
def remove_texinfo (title):
    return texinfo_re.sub (r'\1', title)

def create_texinfo_anchor (title):
    return texinfo_file_name (remove_texinfo (title))

unnumbered_re = re.compile (r'unnumbered.+|lydoctitle')
file_name_section_level = {
    'top': 4,
    'chapter':3,
    'unnumbered':3,
    'appendix':3,
    'section':2,
    'unnumberedsec':2,
    'appendixsec':2,
    'subsection':1,
    'unnumberedsubsec':1,
    'appendixsubsec':1,
    'subsubsection':0,
    'unnumberedsubsubsec':0,
    'appendixsubsubsec':0
}
if split in file_name_section_level:
    splitting_level = file_name_section_level[split]
else:
    splitting_level = -1
def process_sections (filename, lang_suffix, page):
    sections = section_translation_re.findall (page)
    basename = os.path.splitext (os.path.basename (filename))[0]
    p = os.path.join (outdir, basename) + lang_suffix + '.xref-map'
    print 'writing:', p
    f = open (p, 'w')

    this_title = ''
    this_filename = 'index'
    this_anchor = ''
    this_unnumbered = False
    had_section = False
    for sec in sections:
        if sec[0] == "node":
            # Write out the cached values to the file and start a new
            # section:
            if this_title and this_title != 'Top':
                    f.write (this_title + "\t" + this_filename + "\t" + this_anchor + "\n")
            had_section = False
            this_title = remove_texinfo (sec[1])
            this_anchor = create_texinfo_anchor (sec[1])
            # delete entry from master map file
            if this_title in initial_map:
                del initial_map[this_title]
        elif sec[0] == "translationof":
            (original_node, external_node) = external_node_re.subn ('', sec[1])
            original_node = remove_texinfo (original_node)
            # The following binds the translator to use the
            # translated node name in cross-references in case
            # it exists
            if external_node and original_node in initial_map:
                del initial_map[original_node]
            anchor = create_texinfo_anchor (sec[1])
            # If @translationof is used, it gives the original
            # node name, which we use for the anchor and the file
            # name (if it is a numbered node)
            this_anchor = anchor
            if not this_unnumbered:
                this_filename = anchor
            elif original_node in initial_map:
                this_filename = initial_map[original_node][2]
        else:
            # Some pages might not use a node for every section, so
            # treat this case here, too: If we already had a section
            # and encounter another one before the next @node, we
            # write out the old one and start with the new values
            if had_section and this_title:
                f.write (this_title + "\t" + this_filename + "\t" + this_anchor + "\n")
                this_title = remove_texinfo (sec[1])
                this_anchor = create_texinfo_anchor (sec[1])
            had_section = True

            if split == 'custom':
                # unnumbered nodes use the previously used file name,
                # only numbered nodes get their own filename! However,
                # top-level @unnumbered still get their own file.
                this_unnumbered = unnumbered_re.match (sec[0])
                if not this_unnumbered:
                    this_filename = this_anchor
            elif split == 'node':
                this_filename = this_anchor
            else:
                if sec[0] in file_name_section_level and \
                        file_name_section_level[sec[0]] >= splitting_level:
                    this_filename = this_anchor

    if this_title and this_title != 'Top':
        f.write (this_title + "\t" + this_filename + "\t" + this_anchor + "\n")

    for node in initial_map:
        f.write ("\t".join (initial_map[node]) + "\n")
    f.close ()

xref_map_line_re = re.compile (r'(.*?)\t(.*?)\t(.*?)$')
if master_map_file:
    for line in open (master_map_file):
        m = xref_map_line_re.match (line)
        if m:
            initial_map[m.group (1)] = (m.group (1), m.group (2), m.group (3))

for filename in files:
    print "extract_texi_filenames.py: Processing %s" % filename
    (lang_suffix, sections) = extract_sections (filename)
    process_sections (filename, lang_suffix, sections)
