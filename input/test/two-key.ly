\version "1.7.18"

\header { texidoc = "If you specify two different key sigs at one point, a
  warning is printed.  DELETE.

regression?  -gp

no, it doesn't test notation. Perhaps no-notation, but
delete is also fine with me. -hwn
"
}

\score { \notes
\context Voice <
 { \key cis \major cis4 \key bes \major bes4 }
 { \key cis \major fis4 \key es \major g4 }  
>
}
%% new-chords-done %%
