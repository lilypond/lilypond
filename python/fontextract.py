import re

import getopt
import sys
import os

dsr_font_regex = re.compile ('%%DocumentSuppliedResources: font (.*)')
begin_font_regex = re.compile ('%%BeginFont: (.*)')
end_font_regex = re.compile ('%%EndFont')
verbose = 0

try:
    import gettext
    gettext.bindtextdomain ('lilypond', localedir)
    gettext.textdomain ('lilypond')
    _ = gettext.gettext
except:
    def _ (s):
        return s

def scan_files (files):
    file_of_font_dict = {}
    for f in files:
        if verbose:
            sys.stderr.write (_('Scanning %s') % f + '\n')

        header = open (f, 'r').read ()
        idx = 0

        extract_from_this = []
        while idx < len (header):
            match = dsr_font_regex.search (header[idx:])
            if not match:
                break
            name = match.group (1)
            idx += match.end (1)
            if file_of_font_dict.has_key (name):
                continue

            file_of_font_dict[name] = f

    return file_of_font_dict

def get_file_fonts_dict (file_of_font_dict):
    dict = {}
    for (n, f) in file_of_font_dict.items ():
        if not dict.has_key (f):
            dict[f] = []

        dict[f].append (n)

    return dict

def extract_fonts_from_file (extract_from_this, font_dict, filename):
    if extract_from_this:
        curr_font = []
        curr_font_name = []
        in_font = 0
        for l in open (filename).readlines ():
            if not in_font and begin_font_regex.match (l):
                in_font = 1
                curr_font_name = begin_font_regex.match (l).group (1)
                curr_font = []
            elif in_font and end_font_regex.match (l):
                in_font = 0

                if curr_font_name in extract_from_this:
                    font_dict[curr_font_name] = ''.join (curr_font)
                    if verbose:
                        sys.stderr.write (_('Extracted %s')
                                          % curr_font_name + '\n')

                    extract_from_this.remove (curr_font_name)
            elif in_font:
                curr_font.append (l)
            if not extract_from_this:
                break

        if extract_from_this:
            sys.stderr.write ("Failed to extract %s from %s\n"
                              % (', '.join (extract_from_this), filename))

def write_extracted_fonts (output_file_name, font_dict):
    if verbose:
        sys.stderr.write( _('Writing fonts to %s') % output_file_name + '\n')
    output = open (output_file_name, 'w')
    output.write ('''%!PS-Adobe-3.0
%%VMusage: 0 0 
%%Creator: lilypond-extract-fonts
''')

    for x in font_dict.keys ():
        output.write ('%%%%DocumentSuppliedResources: font %s\n' % x)

    output.write ('''%%EndComments\n''')

    for (k,v) in font_dict.items ():
        output.write ('\n%%%%BeginFont: %s\n' % k)
        output.write (v)
        output.write ('\n%%EndFont')


def extract_fonts (output_file_name, input_files):
    d = scan_files (input_files)
    ff = get_file_fonts_dict (d)

    font_dict = {}
    for (file, fonts) in ff.items ():
        extract_fonts_from_file (fonts, font_dict, file)

    write_extracted_fonts (output_file_name, font_dict)


if __name__ == '__main__':
    extract_fonts ('fonts.ps', sys.argv[1:])
