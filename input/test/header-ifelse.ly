\version "2.1.26"


#(define (my-ly-version)
    (list-head (ly:version) 3))

#(if (not (defined? 'pieceTagLine))
    (define pieceTagLine (string-append "Jeremie " (numbers->string (my-ly-version)) " was here")))

\header{
tagline = \pieceTagLine
texidoc = "

High level functionality (eg. conditional defines),
can be accomplished with GUILE.

This example puts the current version in the tagline via Scheme,
however, the tagline is not printed to the collated webpage snippets.

" 
}

\score{ \notes{ c'4 }
\paper {raggedright=##t}
}

