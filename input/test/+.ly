\version "1.9.8"
%% +.ly: Be the first .ly file for lys-to-tely.py.
%% Better to make lys-to-tely.py include "introduction.texi" or
%% other .texi documents too?

\header{
texidoc = "
@section Introduction

This document tests all kinds of features, from simple to advanced,
that are not really suited for the reference manual, and are not
needed as a regression test.

Here you may also find dirty tricks, or very the very latest features
that have not been documented or fully implemented yet.

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
