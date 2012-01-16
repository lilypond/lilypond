# -*- coding: utf-8 -*-

import lilylib as ly
import book_snippets as BookSnippet
from book_snippets import *
import re
global _;_=ly._

progress = ly.progress
warning = ly.warning
error = ly.error

########################################################################
# Helper functions
########################################################################

def find_file (name, include_path, working_dir=None, raise_error=True):
    current_path = working_dir or os.getcwd();
    for i in [current_path] + include_path:
        full = os.path.join (i, name)
        full = os.path.normpath (os.path.join (current_path, full))
        if os.path.exists (full):
            return full

    if raise_error:
        error (_ ("file not found: %s") % name + '\n')
        exit (1)
    return ''

def verbatim_html (s):
    return re.sub ('>', '&gt;',
           re.sub ('<', '&lt;',
               re.sub ('&', '&amp;', s)))


########################################################################
# Option handling
########################################################################

#TODO: Definitions just once in all files!!!
LINE_WIDTH = 'line-width'

# TODO: Implement the intertext snippet option:
#         'intertext': r',?\s*intertext=\".*?\"',

default_snippet_opts = { 'alt': "[image of music]" }


########################################################################
# format handling
########################################################################

all_formats = []
def register_format (fmt):
  all_formats.append (fmt)



########################################################################
# Snippet handling
########################################################################

# Use this for sorting the keys of the defined snippet types (the dict
# is unsorted, so we need to return sorted keys to ensure processing
# in a pre-defined order)
# Containing blocks must be first, see find_toplevel_snippets.

snippet_type_order = [
    'multiline_comment',
    'verbatim',
    'verb',
    'lilypond_block',
    'singleline_comment',
    'lilypond_file',
    'include',
    'lilypond',
    'lilypondversion',
]



########################################################################
# Base class for all output formats
########################################################################

class BookOutputFormat:
    def __init__ (self):
        self.format = None
        self.default_extension = None
        self.snippet_res = {}
        self.output = {}
        self.handled_extensions = []
        self.image_formats = "ps,png"
        self.global_options = {}
        self.document_language = ''
        self.default_snippet_options = default_snippet_opts
        self.snippet_option_separator = "\s*,\s*"

    def supported_snippet_types (self):
        # Sort according to snippet_type_order, unknown keys come last
        keys = self.snippet_res.keys ()
        # First the entries in snippet_type_order in that order (if present)
        # then all entries not in snippet_type_order in given order
        res = filter (lambda x:x in keys, snippet_type_order) + filter (lambda x:x not in snippet_type_order, keys)
        return res

    def snippet_regexp (self, snippettype):
        return self.snippet_res.get (snippettype, None)

    def can_handle_format (self, format):
        return format == self.format
    def can_handle_extension (self, extension):
        return extension in self.handled_extensions

    def add_options (self, option_parser):
        pass

    def process_options (self, global_options):
        pass


    def process_options_pdfnotdefault (self, global_options):
        ## prevent PDF from being switched on by default.
        global_options.process_cmd += ' --formats=eps '
        if global_options.create_pdf:
            global_options.process_cmd += "--pdf -dinclude-eps-fonts -dgs-load-fonts "
            if global_options.latex_program == 'latex':
                    global_options.latex_program = 'pdflatex'


    def snippet_class (self, type):
      return BookSnippet.snippet_type_to_class.get (type, BookSnippet.Snippet)

    def get_document_language (self, source):
        return ''


    def init_default_snippet_options (self, source):
        self.document_language = self.get_document_language (source)
        if LINE_WIDTH not in self.default_snippet_options:
            line_width = self.get_line_width (source)
            if line_width:
                self.default_snippet_options[LINE_WIDTH] = line_width

    def get_line_width (self, source):
        return None;

    def split_snippet_options (self, option_string):
        if option_string:
            return re.split (self.snippet_option_separator, option_string)
        return []

    def input_fullname (self, input_filename):
        return find_file (input_filename, self.global_options.include_path,
            self.global_options.original_dir)

    def adjust_snippet_command (self, cmd):
        return cmd

    def process_chunks (self, chunks):
        return chunks

    def snippet_output (self, basename, snippet):
        warning (_("Output function not implemented"))
        return ''

    def output_simple (self, type, snippet):
        return self.output.get (type, '') % snippet.get_replacements ()

    def output_simple_replacements (self, type, variables):
        return self.output.get (type, '') % variables

    def output_print_filename (self, basename, snippet):
        str = ''
        rep = snippet.get_replacements ()
        if PRINTFILENAME in snippet.option_dict:
            rep['base'] = basename
            rep['filename'] = os.path.basename (snippet.filename)
            rep['ext'] = snippet.ext
            str = self.output[PRINTFILENAME] % rep

        return str

    def required_files (self, snippet, base, full, required_files):
        return []

    def required_files_png (self, snippet, base, full, required_files):
        # UGH - junk global_options
        res = []
        if (base + '.eps' in required_files and not snippet.global_options.skip_png_check):
            page_count = BookSnippet.ps_page_count (full + '.eps')
            if page_count <= 1:
                res.append (base + '.png')
            else:
                for page in range (1, page_count + 1):
                    res.append (base + '-page%d.png' % page)
        return res
