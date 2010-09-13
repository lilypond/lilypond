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

# sanity check for debugging
#for node in nodes:
#    print ' '*2*node[0] + node[1]

def getMenuFor(node_name):
    i = 0
    while nodes[i][1] != node_name:
        i += 1
    startIndex = i + 1
    findType = nodes[i][0] + 1
    menu = []
    for i in range(startIndex, len(nodes)):
        currentSectionType = nodes[i][0]
        currentNodeName = nodes[i][1]
        if currentSectionType < findType:
            break
        elif currentSectionType == findType:
            menu.append(currentNodeName)
        else:
            pass
    return menu

# rewrite file with new menus from TOC
outfile = open(infile, 'w')

lastNode = ''
line_index = 0
while line_index < len(lines):
    line = lines[ line_index ]
    if line.startswith('@node'):
        lastNode = line[6:].rstrip()
    if line.startswith('@menu'):
        outfile.write('@menu\n')
        # skip over existing menu
        # ASSUME: every @menu has a proper @end menu
        while not lines[line_index].startswith('@end menu'):
            line_index += 1

        # write new menu entries
        menu = getMenuFor(lastNode)
        for item in menu:
            node_formatted = '* ' + item + '::\n'
            outfile.write( node_formatted )

        line = lines[line_index]
    line_index += 1
    # write normal line.  Removes tabs and spaces; leaves EOL
    outfile.write(line.rstrip('\t '))
outfile.close()

