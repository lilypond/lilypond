\version "1.3.146"

\header{
texidoc="
Text is set with empty horizontal dimensions.  The boolean property
textNonEmpty is used to respect the horizontal size of text.
"
}
\score { \notes {
\property Voice.TextScript \override #'no-spacing-rods = ##f
c4_"very wide and long text" c4
}

\paper {
  linewidth  = -1.0
  }
}
