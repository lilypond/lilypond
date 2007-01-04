\header {

  texidoc = "This file tests various Scheme utiliity functions." 

}


\version "2.11.8"

%% todo : use macro, to show the statement tested. 
#(ly:progress "~a" (ly:expand-environment "${HOME} $HOME $$ "))
#(ly:font-config-display-fonts)
#(ly:progress "~A" (ly:duration->string (ly:make-duration 2 2 3 7)))


#(ly:parser-parse-string (ly:parser-clone parser) "foo  = #1 #(ly:progress \"hello there\n\")")
