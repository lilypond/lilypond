\version "2.16.0"
\header {
    texidoc = "The layout of the major 7 can be tuned with
@code{majorSevenSymbol}.  It does not break if @code{majorSevenSymbol} is unset.
One should see: triangle - j7 - triangle - #7."
}

\chords {
    c:7+
    \set majorSevenSymbol = \markup { "j7" }
    c:7+
    \unset majorSevenSymbol
    c:7+
    \unset Score.majorSevenSymbol
    c:7+
}
