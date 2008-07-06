\header {

  texidoc = "This file tests various Scheme utility functions." 

}


\version "2.11.51"

%% todo : use macro, to show the statement tested. 
#(ly:progress "~a\n" (ly:expand-environment "${HOME} $HOME $$ "))


%% very platform dependent.
%% #(ly:font-config-display-fonts)

#(ly:progress "~A\n" (ly:duration->string (ly:make-duration 2 2 3 7)))
#(ly:parser-parse-string (ly:parser-clone parser) "foo  = #1 #(ly:progress \"hello there\n\")")

#(ly:progress "~a\n" (ly:truncate-list! (iota 5) 10))
#(ly:progress "~a\n" (ly:truncate-list! (iota 10) 5))
