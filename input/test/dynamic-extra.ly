
\version "2.8.0"
% probably should be merged into refman.
\header{
    texidoc = "@cindex Dynamic Piu Forte
Più forte dynamics is produced using @code{\markup}. " }

piuf =	\markup {  \italic "più" \dynamic "f" }

\layout{ragged-right = ##t}

\relative c''{
  c-\piuf
  c
  c2\< c2\!
  
  c2\< c2\!
}


%% Local Variables:
%% coding: utf-8
%% End:
