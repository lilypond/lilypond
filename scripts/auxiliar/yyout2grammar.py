#! /usr/bin/env python

#  Convert from bison output file parser.output to
#  Grammar and index.
#  Drops all of the state information.
#  Converts \\ to \
#  Eliminates the @ variables created when {} is placed in the middle of a rule##  all of the c-code stuff
#
#
#  Copyright 2005 by Carl D. Sorensen
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
    while inline != '' and not(inline.startswith("Grammar")):
        inline = in_file.readline()
    if inline != '':
        out_file.write(inline)
        inline = in_file.readline()
        while inline != '' and not(inline.startswith("Terminals")):
            i = inline.find("$accept:")
            if i>-1:
                write_me = False
                inline = in_file.readline()
            atfound = re.findall(atre,inline)
            if len(atfound) > 0:
                at_items.extend(atfound)
                print at_items
                write_me = False
                inline=in_file.readline()
            else:
                for at_item in at_items:
                    i=inline.find(at_item)
                    if i >= 0:
                        inline=inline[:i] + inline[i+len(at_item):]
                    i=inline.find('"\\\\')
                    while i > -1 :
                        inline = inline[:i+1]+inline[i+2:]
                        i = inline.find('"\\\\')
            if write_me:
                out_file.write(inline)
            inline = in_file.readline()
            write_me = True
    index_items = []
    #  Write the Terminals header line and the following blank line
    out_file.write(inline)
    inline = in_file.readline()
    out_file.write(inline)
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
        out_file.write(index_item)
    out_file.write('\n')
    # Write the Nonterminals header and the blank line
    out_file.write(inline)
    inline = in_file.readline()
    out_file.write(inline)
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
        out_file.write(index_item)
