\version "2.3.8"


#(if (not (defined? 'pieceTagLine))
  (define pieceTagLine (string-append "Jeremie " (lilypond-version) " was here")))

\header{

    tagline = \pieceTagLine
    texidoc = "

High level functionality (eg. conditional defines),
can be accomplished with GUILE.

This example puts the current version in the tagline via Scheme,
however, the tagline is not printed to the collated webpage snippets.

" 
}

\score{ { c'4 }
\paper {raggedright=##t}
}

