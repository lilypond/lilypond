#  yyout2grammar.py

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2005--2020 by Carl D. Sorensen <c_sorensen@byu.edu>
# Copyright (C) 2019--2020 Robin Bannister <rcb@dabble.ch>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


#  Extract grammar and index from bison output file `parser.output',
#  dropping all of the state information.  Additionally,
#
#  * convert `\\' to `\',
#  * eliminate the `@' variables created when `{}' is placed
#    in the middle of a rule,
#  * eliminate all of the c-code stuff, and
#  * wrap lines that are longer than 78 characters for improved
#    formatting.
#
# Run
#
#   bison -v parser.yy
#
# to create file `parser.output', then run
#
#   yyout2grammar.py parser.output your_output_file

import sys
import re


# for string recognition and modification

mainrulestart_re = re.compile (r' \w*: ') # regular nonterminal on left
auxitem_re = re.compile (r' \$?@\d+') # bison-generated nonterminal on right

intro_re = re.compile (r'(.*[:|])\s')
keyword_re = re.compile (r'(\S+)\s')

# at least bison 2.3 has a buglet that produces a `,' in column 4 within a
# continuation line of a nonterminal; we thus check for three leading spaces
# instead of four
indent34_re = re.compile (r'\A    ?\S') # an indent of 3 or 4 spaces
auxfirstcol_re = re.compile (r'\A[\$@]') # nonterminal starting with $ or @


def rulesplice (rulestartmatch):
    chars = len (rulestartmatch.group (0))
    return "| ".rjust (chars)


def strip_backslash (input_string): # as inserted by bison in its summary
    return input_string.replace ('\\\\', '\\') # double to single


# for writing out our modified summary

# - grammar rule section: break overlong rule lines
# - other sections: their output `lines' already contain suitable breaks
def write_line (output_line, output_file):
    max_line_length = 78
    indent_value = 3
    output_line = strip_backslash (output_line)
    intro = intro_re.match (output_line) # if has a rule start and/or component
    if len (output_line) > max_line_length and intro: # too many items inline
        # give each component item its own line:
        output_file.write (intro.group (1) + " ")
        indent_column = len (intro.group (1))
        output_line = output_line[indent_column:]
        keyword = keyword_re.search (output_line)
        while keyword:
            output_file.write (keyword.group (1) + " \n")
            output_line = output_line[keyword.end (1):]
            keyword = keyword_re.search (output_line)
            if keyword:
                output_file.write ("".rjust (indent_column + indent_value))
    else:
        output_file.write (output_line)
    return


# for reading in the summary produced by bison

# bison may need to insert an aux rule while processing a main rule, and so
# the component lines of a grammar rule may arrive in instalments
#
# `rule_lines' accumulates these instalments until the rule is complete
rule_lines = [""] # a non-printing way to avoid the initial [0].find erroring

def keeprule ():
    global rule_lines
    for rule_line in rule_lines: # move the rule out, but with some items
        write_line (auxitem_re.sub ('', rule_line), out_file) # elided
    rule_lines = [] # and forget it
    write_line ("\n", out_file) # reinsert vert. separator
    return


def getmainrule (rulestarthit):
    global rule_lines, inline
    if rule_lines[0].find (rulestarthit.group (0)) < 0: # mismatch
        # we have reached a different rule, so the previous one is complete
        keeprule () # write out the completed rule, eliding any auxitems
    else:
        # have found a component continuing what is already accumulated
        inline = mainrulestart_re.sub (rulesplice, inline) # hide the breach
    rule_lines.append (inline)
    # now check whether there are any more components
    inline = in_file.readline ()
    while inline != "\n":
        rule_lines.append (inline)
        inline = in_file.readline ()
    # leaving inline found white
    return


def skipto_nonwhite (_inline):
    while _inline == "\n":
        _inline = in_file.readline () # advance
    return _inline


def skipto_white (_inline):
    while _inline != '' and _inline != "\n":
        _inline = in_file.readline () # advance
    return _inline


# script entry point

# if `sys.argv' is lengthened by dummy entries, uncomment this one-liner:
#
# sys.argv.append (''); del sys.argv[sys.argv.index (''):] # prune args of ''

if len (sys.argv) != 3:
    sys.exit ("Usage: yyout2grammar.py parser-output-file grammar-file")
in_name = sys.argv[1]
out_name = sys.argv[2]

in_file = open (in_name, 'r')
out_file = open (out_name, 'w')

# skip preliminary material from file (e.g., unused `Terminals')
inline = in_file.readline ()
while inline != '' and not inline.startswith ("Grammar"):
    inline = in_file.readline ()
# have skipped all preliminary lines

write_line (inline, out_file) # write the `Grammar' header
inline = skipto_nonwhite (in_file.readline ())
# leaving inline at first entry of `Grammar' section
while inline != '' and not inline.startswith ("Terminals"):
    # generally expecting inline to be holding a rule start,
    # but this is not required initially; the loop will phase-lock
    mainrulestarthit = re.search (mainrulestart_re, inline)
    if mainrulestarthit: # found a main rule (re)starting
        getmainrule (mainrulestarthit) # a nonwhite section, maybe partial
    else:
        # found maybe an aux rule start, or just an intermediate line,
        # so discard lines until reach the next probable rule start:
        inline = skipto_white (in_file.readline ())
    inline = skipto_nonwhite (in_file.readline ())
# past end of `Grammar' section

keeprule () # flush the rule buffer; it is waiting for further instalments
if inline == '': # readline encountered EOF recently
    # don't know whether have processed all `Grammar' lines
    sys.exit ("\nGrammar section aborted at EOF\n")

write_line ("\n\n", out_file)

write_line (inline, out_file) # write the `Terminals' header
inline = skipto_nonwhite (in_file.readline ())
# leaving inline at first entry of `Terminals' section
index_items = []
index_item = inline
inline = in_file.readline ()
while inline != '' and not inline.startswith ("Nonterminals"):
    while re.search (indent34_re, inline): # at a continuation line
        index_item = index_item + inline # concatenate it (includes \n)
        inline = in_file.readline ()
    # reached unindented stuff, so `index_item' group is complete
    index_items.append (index_item) # note it
    index_item = inline
    inline = in_file.readline ()
# past end of `Terminals' section

index_items.sort (key = str.lower) # alphabetic
for index_item in index_items:
    write_line (index_item, out_file)
if inline == '': # readline encountered EOF recently
    # don't know whether have processed all `Terminals' lines
    sys.exit ("\n`Terminals' section aborted at EOF\n")

write_line ('\n\n', out_file)

write_line (inline + "\n", out_file) # write the `Nonterminals' header
inline = skipto_nonwhite (in_file.readline ())
# leaving inline at first entry of `Nonterminals' section
index_items = []
index_item = inline
inline = in_file.readline ()
while inline != '' and inline != "\n": # at a nonwhite line
    while re.search (indent34_re, inline): # at a continuation line
        index_item = index_item + inline # concatenate it (includes \n)
        inline = in_file.readline ()
    # reached unindented, so `index_item' group is complete, but...
    if not re.search (auxfirstcol_re, index_item):
        # ...only main nonterminals are noted
        index_items.append (index_item)
    index_item = inline
    inline = in_file.readline ()
# past end of (wholly nonwhite) `Nonterminals' section

index_items.sort (key = str.lower) # alphabetic
for index_item in index_items:
    write_line (index_item, out_file)
if inline == '': # readline encountered EOF recently
    # don't know whether have processed all `Nonterminals' lines
    sys.exit ("\n`Nonterminals' section aborted at EOF\n")

# eof
