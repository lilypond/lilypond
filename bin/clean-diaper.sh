#!/bin/sh

# unclobber current dir.
rm -vf *.aux *.log *.orig *~
rm -vf `grep -li "^% Creator: GNU LilyPond" *.out *.tex` 
rm -vf `grep -li "Creator: mi2mu" *.midi.ly`
rm -vf `grep -li "%Creator: dvipsk" *.ps`
rm -vf `grep -li "Creator: GNU LilyPond" *.midi`
rm -vf `grep -li "Creator: ly2dvi" *.tex`
rm -vf `find -name 'core'`
rm -vf `find -name '*.orig'`
rm -vf `find -name '*.rej'`

    
# docxx mess
rm -vf *dvi
rm -vf *.class  HIER*.html dxxgifs.tex gifs.db icon?.gif logo.gif down.gif \
    aindex.html index.html
