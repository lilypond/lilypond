\version "2.11.51"
%% +.ly: Be the first .ly file for lys-to-tely.py.
%% Better to make lys-to-tely.py include "introduction.texi" or
%% other .texi documents too?


\header{
texidoc =

#(string-append "@unnumbered Introduction

This document presents proofs for
LilyPond " (lilypond-version) ".  When the
text corresponds with the shown notation, we consider LilyPond Officially
BugFree (tm).  This document is intended for finding bugs and for
documenting bugfixes.

In the web version of this document, you can click on the file name 
or figure for each example to see the corresponding input file.

TODO: order of tests (file names!), test only one feature per test.
Smaller and neater tests.
")

}

%
% make sure the .png is generated.
%
\lyrics { "(left blank intentionally)" }

