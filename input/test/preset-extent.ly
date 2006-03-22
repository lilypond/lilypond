\version "2.8.0"
\header { texidoc = "

@cindex Preset Extent

The object may be extended to larger sized by overriding their properties.
The lyrics in this example have an extent of @code{(-10,10)}, which is why 
they are spaced so widely.

"

}

\score {
    \context Lyrics \lyricmode {
	foo --
	
	\override LyricText  #'X-extent = #'(-10.0 . 10.0)
 bar baz
	}
    \layout { ragged-right = ##t}
}
    

