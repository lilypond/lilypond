\version "1.3.146"

\header{
texidoc="
@c FIXME
Similarly, if @code{stem_default_neutral_direction} is set to @code{-1}.
"
}

\score{
	\notes\relative c{
	    b''4 ~ b8()b8 e4 e,
	}
	\paper{
		stem_default_neutral_direction=-1.0
	}
}
