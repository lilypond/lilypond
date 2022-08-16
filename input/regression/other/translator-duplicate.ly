\version "2.23.13"

\header {
  texidoc = "Only one instance of a translator can exist in a context.
Duplicates are eliminated."
}

%% Test behavior with \consists <symbol>

#(ly:register-translator
  (lambda (context)
    (make-engraver
     ((initialize engraver)
      (ly:message "This message should appear only once."))))
  'Dummy_engraver
  '((events-accepted) (grobs-created) (properties-read) (properties-written)))

%% Test behavior with \consists <procedure>

#(define (Dummy_engraver_2 context)
   (make-engraver
    ((initialize engraver)
     (ly:message "This is a second message that should appear only once."))))

myCommand = \with { \consists Dummy_engraver }

\layout {
  \context {
    \Voice
    \myCommand
    \consists Dummy_engraver
    \consists #Dummy_engraver_2
  }
}

\new Voice \with {
  \consists #Dummy_engraver_2
}
{ c' }

% A single \remove cancels both \consists
\new Voice \with {
  \remove Dummy_engraver
  \remove #Dummy_engraver_2
}
{ c' }
