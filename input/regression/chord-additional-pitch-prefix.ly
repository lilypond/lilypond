\version "2.21.0"
\header {
    texidoc = "The prefix of additional chord pitches can be tuned with
@code{additionalPitchPrefix}."
}

\new ChordNames {
    <c e g d'>	% add9
    \set additionalPitchPrefix = "add"
    <c e g d'>	% add9
}
