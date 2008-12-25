\version "2.12.0"

"expect-error" = ##t

% Ugh - this affects other files too.
#(ly:set-option 'protected-scheme-parsing #t)
#(ly:set-option 'safe #t)

"force-finish" = ##t  

\header{
    texidoc = "This should not survive lilypond --safe-mode"

    #(open-file "w")
}


#(open-file "/tmp/safe-guile.scm")

\include "this-should-fail.ly"

