\version "1.9.8"
%% +.ly: Be the first .ly file for lys-to-tely.py.
%% Better to make lys-to-tely.py include "introduction.texi" or
%% other .texi documents too?


\header{
texidoc = "
@section Introduction

This document presents a brief overview of LilyPond features.  When the
text correspond with the shown notation, we consider LilyPond Officially
BugFree (tm).  This document is intended for finding bugs, and
documenting bugfixes.

TODO: order of tests (file names!), test only one feature per test.
Smaller and neater tests.


"

}

\score { \context Lyrics \notes {
    \property Score.RehearsalMark \set #'self-alignment-X = #LEFT

\mark #(ly:export    (string-append "(For LilyPond version "
(lilypond-version) ")"))
s2
 }
 \paper { indent = 0.0\pt
 raggedright   = ##t 
 }
}
