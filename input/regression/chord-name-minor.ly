\version "2.15.16"
\header {
    texidoc = "The layout of the minor chord can be tuned with
@code{minorChordModifier}."
}

\chords {
    c:min
    c:min7
    \set minorChordModifier = \markup { "-" }
    c:min
    c:min7
}
