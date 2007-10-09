\version "2.10.0"
%% +.ly: Be the first .ly file for lys-to-tely.py.
%% Better to make lys-to-tely.py include "introduction.texi" or
%% other .texi documents too?


\header{
texidoc =

#(string-append "@unnumbered MusicXML regression and coverage test

This document presents proofs for the musicxml2ly script provided with
LilyPond " (lilypond-version) ". The files don't have a description yet, so 
there is no official indication when the output is wrong.
These snippets are provided as unit test files in MusicXML, converted to
a .ly file by musicxml2ly and then processed by lilypond as usual.

If something does not seem wright in the output, it might either be that this
feature has not been implemented yet, has been wrongly implemented, or a regression
has crept in recently...
This document is intended for finding bugs and for documenting bugfixes.

In the web version of this document, you can click on the file name 
or figure for each example to see the corresponding .ly intermediary file.

TODO: Find a way to add a description to the .ly files for texinfo.

The files are categorized by their first two digits with the following meaning:
@itemize
@item 00 ... Basic notes notation (pitches, durations, accidentals, etc.)
@item 01 ... Chords
@item 02 ... Articulations, Ornaments, Technicals
@item 03 ... Dynamics (single symbols, not spanners like hairpins)
@item 04 ... Spanners (hairpins, octave shifts, trills, etc.)
@item 05 ... Header information (title, composer, poet, etc.)
@item 06 ... Lyrics
@item 07 ... Clefs, keys, upbeats
@item 08 ... Multiple parts (staves)
@item 09 ... Repeats, barlines
@item 10 ... Multiple voices per staff
@item 11 ... Rests (durations, pitched rests, etc.)
@item 12 ... Triplets, Tuplets
@item 13 ... Grace notes
@item 14 ... Multi-staff parts (one voice on multiple staves, e.g. PianoStaff)
@item 15 ... Percussion
@end itemize
")

}

%
% make sure the .png is generated.
%
\lyrics { "(left blank intentionally)" }