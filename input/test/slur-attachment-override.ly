
\version "2.3.4"

\header{
texidoc="@cindex Slur Attachment Override
In some cases, you may want to control the attachment points of a slur 
by hand. "
}


%%
%% except that both slurs are stem <<-> stem.
%%

fragment =  {
  \set autoBeaming = ##f
  \override Stem  #'direction = #1
  \override Slur  #'direction = #1
  d'32( f'4  d8..)
  \override Slur  #'attachment = #'(stem . stem)
  d,32( f'4  d8.)
}


\score {
	\relative c \fragment
	\paper { raggedright = ##t} 
}

