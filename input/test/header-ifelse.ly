\version "2.3.16"


#(if (not (defined? 'pieceTagLine))
  (define pieceTagLine (string-append "Title has version " (lilypond-version))))

\header{
    title = \pieceTagLine
    texidoc = "

High level functionality (eg. conditional defines), can be
accomplished with GUILE.

This example puts the current version in the title via Scheme.

" 
}

\paper {raggedright=##t}
{ c'4 }

