\version "2.10.0"
%% +.ly: Be the first .ly file for lys-to-tely.py.
%% Better to make lys-to-tely.py include "introduction.texi" or
%% other .texi documents too?

\header{
texidoc = #(string-append "
@section Introduction

This document shows all kinds of advanced snippets from the
LilyPond Snippet Repository.

In the web version of this document, you can click on the file name
or figure for each example to see the corresponding input file.

This document is for LilyPond version 
" (lilypond-version) ".")
}

% make sure .png  is generated.
\lyrics {  "(left blank intentionally)" }
