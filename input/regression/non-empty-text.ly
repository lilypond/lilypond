\version "1.3.146"

\header{
texidoc="
Text is set with empty horizontal dimensions.  The boolean property
textNonEmpty is used to respect the horizontal size of text.
"
}
\score { \notes {
c2_"very wide and long text" c | \break
\fatText  % short for \property Voice.textNonEmpty = ##t
c_"very wide and long text" c
}

\paper {
  linewidth  = 3.\cm
  }
}
