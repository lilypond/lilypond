\version "2.1.19"
\header { texidoc = "

@cindex Preset Extent


Grob extents may be hard coded using grob properties.  This
requires Grob::preset_extent () function.

The lyrics in this example have extent (-10,10) which is why they are
spaced so widely.

"

}

\score {
    \context LyricsVoice \lyrics {
	foo --
	\property LyricsVoice . LyricText \set #'X-extent-callback = #Grob::preset_extent
	\property LyricsVoice . LyricText \set #'X-extent = #'(-10.0 . 10.0)
 bar baz
	}
    \paper { raggedright = ##t}
}
    

