#!/bin/sh

rm -f feta-sleur-[0-9x\-]*.mf
rm -f feta-sleur-[0-9x\-]*.log
rm -f feta-beum-[0-9x\-]*.mf
rm -f feta-beum-[0-9x\-]*.log
rm -f feta-embed.aux
rm -f missfont.log
clean-fonts sleur
clean-fonts beum
