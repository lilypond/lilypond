\header  {
texidoc = "New markup syntax."

}
\version "1.7.8"


\score {
  \notes \transpose c d
%\apply #display-music
   {
    \property Voice.TextScript \set #'molecule-callback = #brew-new-markup-molecule
    eses'-\markup { foo \bold bar \column < baz bazr >
		\override #'(font-family . music) \lookup #"noteheads-0"
	} }
}
