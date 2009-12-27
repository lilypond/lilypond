#! /usr/bin/env python
#
#  yyout2grammar.py

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2005 by Carl D. Sorensen <c_sorensen@byu.edu>
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


#  Convert from bison output file parser.output to
#  Grammar and index.
#  Drops all of the state information.
#  Converts \\ to \
#  Eliminates the @ variables created when {} is placed
#        in the middle of a rule
#  Eliminates all of the c-code stuff
#  Wraps lines that are longer than 78 characters for improved
#        formatting
#
# to create input file, run
#   bison -v parser.yy
# this will create a file parser.output
# then run
#   yyout2grammar.py parser.output your_output_file
#

import sys
import re

atre = re.compile('(@\d+):')

intro_re = re.compile (r'(.*[:|])\s')
keyword_re = re.compile (r'(\S+)\s')

# strip extra backslashes that are inserted by the python
# string handling routines
def strip_backslash(input_string):
    i=input_string.find(r'"\\')
    while i > -1 :
        input_string = input_string[:i+1]+input_string[i+2:]
        i = input_string.find(r'"\\')
    return  input_string

#  write an output line, adjusting to make sure that max_line_length
#  is not exceeded
def write_line (output_line, output_file):
    max_line_length = 78
    indent_value = 3
    if len(output_line) > max_line_length:
        intro = intro_re.match(output_line)
        if intro:
            output_file.write(intro.group(1)+" ")
            indent_column = len(intro.group(1))
            output_line = output_line[indent_column:]
            keyword = keyword_re.search(output_line)
            while keyword:
                output_file.write(strip_backslash(keyword.group(1))+" \n")
                output_line = output_line[keyword.end(1):]
                keyword = keyword_re.search(output_line)
                if keyword:
                    output_file.write("".rjust(indent_column + indent_value))
    else:
        output_file.write(strip_backslash(output_line))
    return

write_me = True

if len(sys.argv)!=3:
    print "Usage: yyout2grammar.py parser_output_file grammar_file."
else:
    in_name = sys.argv[1]
    out_name = sys.argv[2]

    print "input file name",in_name
    print "output file name",out_name
    in_file = open(in_name,'r')
    out_file= open(out_name, 'w')

    at_items=[]
    inline = in_file.readline()

    ## skip header material from file
    while inline != '' and not(inline.startswith("Grammar")):
        inline = in_file.readline()

    ## process the Grammar lines
    if inline != '':
        write_line(inline, out_file)
        inline = in_file.readline()
        while inline != '' and not(inline.startswith("Terminals")):
            i = inline.find("$accept:")
            if i>-1:
                write_me = False
                inline = in_file.readline()
            atfound = re.findall(atre,inline)
            if len(atfound) > 0:
                at_items.extend(atfound)
                #  print at_items
                write_me = False
                inline=in_file.readline()
            else:
                for at_item in at_items:
                    i=inline.find(at_item)
                    ## remove @ item
                    if i >= 0:
                        inline=inline[:i] + inline[i+len(at_item):]

            if write_me:
                write_line(inline, out_file)
            inline = in_file.readline()
            write_me = True
    index_items = []

    #  Write the Terminals header line and the following blank line
    write_line(inline, out_file)
    inline = in_file.readline()
    write_line(inline, out_file)
    inline = in_file.readline()
    while inline != '' and not(inline.startswith("Nonterminals")):
        i=inline.find('"\\\\')
        while i > -1 :
            inline = inline[:i+1]+inline[i+2:]
            i = inline.find('"\\\\')
        index_items.append(inline)
        inline = in_file.readline()
    index_items.sort(lambda x,y:cmp(x.lower(),y.lower()))
    for index_item in index_items:
        write_line (index_item, out_file)
    write_line ('\n', out_file)

    # Write the Nonterminals header and the blank line
    write_line(inline, out_file)
    inline = in_file.readline()
    write_line(inline, out_file)
    index_items = []
    index_item=in_file.readline()
    inline=in_file.readline()
    while inline != '' and not(inline.startswith("state")):
        while inline.startswith("    "):
            index_item = index_item + inline
            inline = in_file.readline()
        if not(index_item.startswith("@")) and \
               not(index_item.startswith("$accept")):
            index_items.append(index_item)
        index_item = inline
        inline=in_file.readline()
    index_items.sort(lambda x,y:cmp(x.lower(),y.lower()))
    for index_item in index_items:
        write_line (index_item, out_file)
