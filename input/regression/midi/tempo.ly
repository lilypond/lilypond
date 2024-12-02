\version "2.25.23"

\header {
  texidoc = "The @code{\\tempo} command changes the tempo in MIDI output.
Verifying this requires manual inspection."
}

\score {
  \fixed c' {
    %% 1/256 quarter note per minute = 15360000000 µs / quarter note.
    %% A standard MIDI file can specify at most 16777215 µs / quarter note.
    \tempo 1024 = 1
    s1
    %% 15/4 quarter notes per minute = 16000000 µs / quarter note.
    \tempo 16 = 15
    s1
    \tempo 4 = 32768 % = 1831.05 µs / quarter note
    s1
    \tempo 4 = 33554432 % = 1.79 µs / quarter note.
    s1
    %% 134217728 quarter notes per minute = 0.45 µs / quarter note.
    \tempo \maxima = 4194304
    s1
  }

  \midi {}
}
