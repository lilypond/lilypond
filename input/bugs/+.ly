%% +.ly: Be the first .ly file for lys-to-tely.py.
%% Better to make lys-to-tely.py include "introduction.texi" or
%% other .texi documents too?


\header{
texidoc = "
@section Introduction

This document presents brief overview of all simple bugs known to
exist in LilyPond.  Things that don't look like bugs, have hopefully
been fixed.  Note that this page only shows notational bugs, input
that does not result in any notation (ie, crashes lilypond) should go
in @file{input/no-notation/}.

"

foollilypondbook = "
\score
"
}

\score{
\context Lyrics \lyrics { "." }
}
