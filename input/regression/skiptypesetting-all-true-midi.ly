\version "2.25.19"

\header{
  texidoc = "
A score with @code{skipTypesetting} set for the whole score
will not segfault.
"
}

%% TODO: Move this case into input/regression/midi?  The challenge is that it
%% doesn't create a MIDI file and our test framework doesn't currently support
%% expecting that.

%% TODO: Expect the warning "cannot create a zero-track MIDI file; skipping
%% `%s'".  The challenge is that the file name is chosen by a script, it might
%% vary as tests are added or removed, and our error matcher doesn't offer any
%% way to gloss over it.

\score {
  {
    \set Score.skipTypesetting = ##t
    c1
  }
  \midi { }
}
