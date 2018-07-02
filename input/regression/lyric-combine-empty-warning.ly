\version "2.19.2"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "cannot find context: %s") "Voice = not-existing-notes")

\header {

  texidoc = "If lyrics are assigned to a non-existing voice, a warning should
be printed.  However, if the lyrics context does not contain any lyrics, then
no warning should be printed."

}

<<
  \new Staff
    \new Voice = "notes" {
      c1
    }
  % This should not give a warning (empty lyrics, existing voice):
  \new Lyrics \lyricsto "notes" { }
  % This should give a warning (non-existing voice):
  \new Lyrics \lyricsto "not-existing-notes" { Test }
  % This should NOT give a warning (non-existing voice, but also no lyrics):
  \new Lyrics \lyricsto "not-existing-notes" { }
>>
