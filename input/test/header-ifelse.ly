% #(define pieceTagLine "Copyright 2002 (C) Mutopia")
\version "1.7.20"
% FIXME: since tagline isn't used in creating the webpage, this example
% doesn't output anything unusual.

#(define (my-ly-version)
  (let ((version (ly:version)))
    (list (car version) (cadr version) (caddr version))))

#(if (not (defined? 'pieceTagLine))
    (define pieceTagLine (string-append "Jeremie " (numbers->string (my-ly-version)) " was here")))

pieceTagLine = #pieceTagLine
\header{
tagline = \pieceTagLine
texidoc = "@cindex Header If Else
High level functionality can be accomplished with GUILE. Semantics aren't nice though." 
}

\score{ \notes{ c4 }
\paper {raggedright=##t}
}
%% new-chords-done %%
