\version "2.10.0"

"expect-error" = ##t

#(ly:lexer-set-safe! (ly:parser-lexer parser))

"force-finish" = ##t  

\header{
    texidoc = "This should not survive lilypond --safe-mode"

    #(open-file "w")
}


#(open-file "/tmp/safe-guile.scm" "r")

\include "this-should-fail.ly"

