\version "2.16.0"

#(ly:set-option 'warning-as-error #f)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "cannot find Voice `%s'") "not-existing-notes")

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
  \new Lyrics \lyricsto "notes" \lyricmode { }
  % This should give a warning (non-existing voice):
  \new Lyrics \lyricsto "not-existing-notes" \lyricmode { Test }
  % This should NOT give a warning (non-existing voice, but also no lyrics):
  \new Lyrics \lyricsto "not-existing-notes" \lyricmode { }
>>
