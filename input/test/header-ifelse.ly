\version "2.7.13"


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

\layout {raggedright=##t}
{ c'4 }

