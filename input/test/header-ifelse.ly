% #(define pieceTagLine "Copyright 2002 (C) Mutopia")

#(define (my-ly-version)
  (let ((version (ly:version)))
    (list (car version) (cadr version) (caddr version))))

#(if (not (defined? 'pieceTagLine))
    (define pieceTagLine (string-append "Jeremie " (numbers->string (my-ly-version)) " was here")))

pieceTagLine = #pieceTagLine
\header{
tagline = \pieceTagLine
texidoc = "High level functionality can be accomplished with GUILE. Semantics aren't nice though." 
}

\score{ \notes{ c4 } }
