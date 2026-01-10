\version "2.25.32"
\header {
    texidoc = "The prefix of additional chord pitches can be tuned with
@code{additionalPitchPrefix}."
}

\new ChordNames {
    <c e g d'>	% add9
    \set additionalPitchPrefix = ""
    <c e g d'>	% add9
}
