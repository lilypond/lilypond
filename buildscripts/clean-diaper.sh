#!/bin/sh

# unclobber current dir.
rm -vf *.aux *.log *.orig *~  *.dvi *.dep

# subdirs
rm -vf `grep -li "^%created by: GNU LilyPond" *.out *.tex`
rm -vf `grep -li "^% *Creator: GNU LilyPond" *.out *.tex` 
rm -vf `grep -li "Creator: mi2mu" *.midi.ly`
rm -vf `grep -li "%Creator: dvipsk" *.ps`
rm -vf `grep -li "Creator: GNU LilyPond" *.midi`
rm -vf `grep -li "Creator: ly2dvi" *.tex`
rm -vf `find -name 'core'`
rm -vf `find -name '*.orig'`
rm -vf `find -name '*.rej'`
rm -vf `find -name 'feta*pk'`
rm -vf `find -name 'feta*tfm'`



    
# docxx mess
rm -vf *.class  HIER*.html dxxgifs.tex gifs.db icon?.gif logo.gif down.gif \
    aindex.html index.html
