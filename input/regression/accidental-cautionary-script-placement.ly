\version "2.23.6"
\header {
 texidoc = "Test if Scripts are placed over notes with accidentals the
            same way as over notes with cautionary accidentals."
}

\layout { ragged-right = ##t }

{
    c'''!->
    \once\override AccidentalCautionary.parenthesized = ##f
    c'''?->
    \once\override Accidental.parenthesized = ##t
    c'''!->
    c'''?->
}
