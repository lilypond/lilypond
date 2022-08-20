%% DO NOT EDIT this file manually; it was automatically
%% generated from the LilyPond Snippet Repository
%% (http://lsr.di.unimi.it).
%%
%% Make any changes in the LSR itself, or in
%% `Documentation/snippets/new/`, then run
%% `scripts/auxiliar/makelsr.pl`.
%%
%% This file is in the public domain.

\version "2.23.12"

\header {
  lsrtags = "pitches"

  texidoc = "
@code{staffLineLayoutFunction} is used to change the position of notes.
This snippet shows setting its value to @code{ly:pitch-semitones} in
order to produce a chromatic scale with the distance between each space
and line of the stave equal to one semitone.
"

  doctitle = "Changing the interval of lines on the stave"
} % begin verbatim


scale = \relative c' {
  a4 ais b c
  cis4 d dis e
  f4 fis g gis
  a1
}

\new Staff \with {
  \remove "Accidental_engraver"
  staffLineLayoutFunction = #ly:pitch-semitones
}
{
  <<
    \scale
    \context NoteNames {
      \set printOctaveNames = ##f
      \scale
    }
  >>
}
