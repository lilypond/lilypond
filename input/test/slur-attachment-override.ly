#(ly:set-option 'old-relative)
\version "1.9.1"

\header{
texidoc="@cindex Slur Attachment Override
In some cases you may want to set slur attachments by hand. "
}


%%
%% except that both slurs are stem <-> stem.
%%

fragment = \notes {
  \property Voice.autoBeaming = ##f
  \property Voice.Stem \set #'direction = #1
  \property Voice.Slur \set #'direction = #1
  d'32( f'4  d8..)
  \property Voice.Slur \set #'attachment = #'(stem . stem)
  d,32( f'4  d8.)
}


\score {
	\notes\relative c \fragment
	\paper { raggedright = ##t} 
}

