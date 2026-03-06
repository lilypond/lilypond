\version "2.25.23"

\header {
  texidoc = "The @code{\\tempo} command changes the tempo in MIDI output.
Verifying this requires manual inspection."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \fixed c' {
    %% (2^24 - 1) µs / quarter note is the limit for a standard MIDI file.
    #(ly:expect-warning (ly:translate-cpp-warning-scheme
                         "Unsupported MIDI tempo (wholes/minute): %s")
      "234375/262144")
    \tempo 4 = #60000000/16777216 % = 16777216 µs / quarter note
    s1
    \tempo 4 = #60000000/16777215 % = 16777215 µs / quarter note
    s1
    %% 15/4 quarter notes per minute = 16000000 µs / quarter note.
    \tempo 16 = 15
    s1
    \tempo 4 = 32768 % = 1831.05 µs / quarter note
    s1
    \tempo 4 = 33554432 % = 1.79 µs / quarter note.
    s1
    \tempo 4 = #60000000/1 % = 1 µs / quarter note
    s1
    %% 134217728 quarter notes per minute = 0.45 µs / quarter note.
    #(ly:expect-warning (ly:translate-cpp-warning-scheme
                         "Unsupported MIDI tempo (wholes/minute): %s")
      "33554432")
    \tempo \maxima = 4194304
    s1
  }

  \midi {}
}
