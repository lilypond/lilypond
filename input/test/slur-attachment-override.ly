
\version "2.1.26"

\header{
texidoc="@cindex Slur Attachment Override
In some cases you may want to set slur attachments by hand. "
}


%%
%% except that both slurs are stem <<-> stem.
%%

fragment = \notes {
  \set autoBeaming = ##f
  \override Stem  #'direction = #1
  \override Slur  #'direction = #1
  d'32( f'4  d8..)
  \override Slur  #'attachment = #'(stem . stem)
  d,32( f'4  d8.)
}


\score {
	\notes\relative c \fragment
	\paper { raggedright = ##t} 
}

