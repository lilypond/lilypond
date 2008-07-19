#!@PYTHON@
# -*- coding: utf-8 -*-
# extrace_texi_filenames.py

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
# texifile_xref.map with tab-separated entries of the form
#        NODE\tFILENAME\tANCHOR
# Note: The filename does not have any extension appended!
# This file can then be used by our texi2html init script to determine 
# the correct file name and anchor for external refs

import sys
import re
import os
import getopt

#import langdefs

optlist, args = getopt.getopt (sys.argv[1:],'o:')
files = args

outdir = '.'
for x in optlist:
    if x[0] == '-o':
        outdir = x[1]

include_re = re.compile (r'@include ((?!../lily-).*?)\.texi$', re.M)
whitespaces = re.compile (r'\s+')
section_translation_re = re.compile (r'@(node|(?:unnumbered|appendix)(?:(?:sub){0,2}sec)?|top|chapter|(?:sub){0,2}section|(?:major|chap|(?:sub){0,2})heading|translationof) (.*?)\n')

def expand_includes (m):
    filepath = os.path.join (os.path.dirname (m.group(0)), m.group(1)) + '.texi'
    print "Including file: " + filepath
    if os.path.exists (filepath):
        return extract_sections (filepath)
    return ''

def extract_sections (filename):
    result = ''
    f = open (filename, 'r')
    page = f.read ()
    f.close()
    # Replace all includes by their list of sections and extract all sections
    page = include_re.sub (expand_includes, page)
    sections = section_translation_re.findall (page)
    for sec in sections:
        result += "@" + sec[0] + " " + sec[1] + "\n"
    return result

# Convert a given node name to its proper file name (normalization as explained
# in the texinfo manual:
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
    if ord(result[0]) in range (ord('0'), ord('9')):
        result = 't_g' + result
    return result

texinfo_re = re.compile (r'@.*{(.*)}')
def remove_texinfo (title):
    return texinfo_re.sub (r'\1', title)

def create_texinfo_anchor (title):
    return texinfo_file_name (remove_texinfo (title))

unnumbered_re = re.compile (r'unnumbered.*')
def process_sections (filename, page):
    sections = section_translation_re.findall (page)
    # TODO: Don't rely on the file having a 4-letter extension (texi)!!!
    p = os.path.join (outdir, filename) [:-5] + '_xref.map'
    f = open (p, 'w')

    this_title = ''
    this_filename = ''
    this_anchor = ''
    this_unnumbered = False
    for sec in sections:
        if sec[0] == "node":
            # Write out the cached values to the file and start a new section:
            if this_title != '':
                f.write (this_title + "\t" + this_filename + "\t" + this_anchor + "\n")
                print (this_title + "\t" + this_filename + "\t" + this_anchor)
            this_title = remove_texinfo (sec[1])
            this_anchor = create_texinfo_anchor (sec[1])
        elif sec[0] == "translationof":
            anchor = create_texinfo_anchor (sec[1])
            # If @translationof is used, it gives the original node name, which
            # we use for the anchor and the file name (if it is a numbered node)
            this_anchor = anchor
            if not this_unnumbered:
                this_filename = anchor
        else:
            # unnumbered nodes use the previously used file name, only numbered
            # nodes get their own filename!
            this_unnumbered = unnumbered_re.match (sec[0])
            if not this_unnumbered:
                this_filename = this_anchor

    if this_title != '':
        f.write (this_title + "\t" + this_filename + "\t" + this_anchor + "\n")
        print (this_title + "\t" + this_filename + "\t" + this_anchor)
    f.close ()


for filename in files:
    print "extract_texi_filenames.py: Processing %s" % filename
    sections = extract_sections (filename)
    process_sections (filename, sections)
