\header {

  texidoc = "In overlapping unisons, within a single MIDI channel,
either the first note is truncated, or the notes are merged if
@code{midiMergeUnisons} is @code{#t}. Run
@code{timidity -idvvv file.midi |grep Midi} to see midi events."

}

\version "2.16.0"

\score {
  {
    \set Score.midiChannelMapping = #'staff
    \new Staff  << {r8 g'4.} \\ {g'4. r8} >>
    \new Staff \with { midiMergeUnisons = ##t } << {r8 a'4.} \\ {a'4. r8} >>
  }
  \midi {}
  \layout {}
}
