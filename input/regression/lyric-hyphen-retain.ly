\header {

  texidoc = "The minimum distance between lyrics is determined by the
@code{minimum-distance} of @code{LyricHyphen} and
@code{LyricSpace}.

The ideal length of a hyphen is determined by its @code{length}
property, but it may be shortened down to @code{minimum-length} in
tight situations. If in this it still does not fit, the hyphen will be
omitted.

Like all overrides within @code{\\lyricsto} and @code{\\addlyrics}, the
effect of a setting is delayed is one syllable."

}

\version "2.19.21"

\layout {
  ragged-right = ##t
}


\relative {
  \time 2/4
  c''32 c c c 
  c32 c c c 
  c32 c c c 
  c32 c c c 
  c32 c c c 
}
\addlyrics
{
  %% tight spacing: hyphen disappears 
  syl -- lab word

  %% increase minimum-distance: forces more space, so hyphen reappears
  \override LyricHyphen.minimum-distance = #1.0
  syl -- lab word

  %% minimum-distance 0 for LyricSpace: this places words next to each other. 
  \override LyricSpace.minimum-distance = #0.0
  syl -- lab word
}
