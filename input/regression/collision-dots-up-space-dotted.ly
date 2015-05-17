\header {

  texidoc = "For collisions where the upper note is dotted and in a
  space, the upper is moved to right.  This behavior can be tuned by
  prefer-dotted-right."

  }

\version "2.19.21"

\paper{ ragged-right=##t }

\new Staff \relative <<
  { fis'4.
    \override Staff.NoteCollision.prefer-dotted-right = ##f
    fis4. }
  \\
  { \autoBeamOff e8 e e e e e e }
>> 
