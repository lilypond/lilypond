#(ly:set-option 'old-relative)
\version "1.9.1"


\header {
texidoc =  
"Beaming can be overidden for individual stems."
}
\score {
\notes \relative c'' {
  c32[ c


%% WARNING: #'beaming is written, so this
%% property can not be shared between objects. Always use
%%  \once.
%%


  \once \property Voice.Stem \override
   #'beaming = #(cons (list   1 2) (list 0 2 4))
c
  \once \property Voice.Stem \override
   #'beaming = #(cons (list 0 2 4) (list 0 1 4))
c c c]
}
\paper{ linewidth = -1.0 }
}
