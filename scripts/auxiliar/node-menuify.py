#!/usr/bin/env python
import sys

try:
    infile = sys.argv[1]
    lines = open(infile).readlines()
except:
    print "ERROR: need a filename"
    sys.exit(1)

nodes = []

# Generate TOC
for i in range(len(lines)):
    line = lines[i]
    if line.startswith('@node '):
        node_name = line[6:].rstrip()
        # ASSUME: the last line of the file is not @node
        section_line = lines[i+1]
        if (section_line.startswith('@chapter ') or
              section_line.startswith('@unnumbered ') or
              section_line.startswith('@appendix ')):
            section_type = 1
        elif (section_line.startswith('@section ') or
              section_line.startswith('@unnumberedsec ') or
              section_line.startswith('@appendixsec ')):
            section_type = 2
        elif (section_line.startswith('@subsection ') or
              section_line.startswith('@unnumberedsubsec ') or
              section_line.startswith('@appendixsubsec ')):
            section_type = 3
        elif (section_line.startswith('@subsubsection ') or
              section_line.startswith('@unnumberedsubsubsec ') or
              section_line.startswith('@appendixsubsubsec ')):
            section_type = 4
        else:
            print "ERROR: unknown sectioning command"
            print section_line
            sys.exit(1)
        nodes.append( (section_type, node_name) )

# rewrite file with new menus from TOC
outfile = open(infile, 'w')
line_index = 0
toc_index = 0
while line_index < len(lines):
    line = lines[ line_index ]
    if line.startswith('@menu'):
        outfile.write('@menu\n')
        # ASSUME: every @menu has a proper @end menu
        while not lines[line_index].startswith('@end menu'):
            line_index += 1

        # write new menu entries
        menu_type = nodes[toc_index][0]
        i = toc_index
        while nodes[i][0] == menu_type:
            i += 1
            if i >= len(nodes):
                break
        added = 0
        while True:
            if i >= len(nodes):
                added = 1
                break
            section_type = nodes[i][0]
            if section_type == menu_type+1:
                node_name = nodes[i][1]
                node_formatted = '* ' + node_name + '::\n'
                outfile.write( node_formatted )
                added += 1
            if section_type == menu_type:
                added += 1
                break
            i += 1
        toc_index += added
        line = lines[line_index]
    line_index += 1
    # write normal line.  Removes tabs and spaces; leaves EOL
    outfile.write(line.rstrip('\t '))
outfile.close()

