\header {

texidoc = "Grob extents may be hard coded using grob
    properties.  This requires Grob::preset_extent () function.
" 
}

\score {
    \context Lyrics \lyrics {
	\property Lyrics . LyricHyphen \set #'extra-offset = #'(0.0 . -5.0) 
	foo --
	\property Lyrics . LyricText \set #'X-extent-callback = #Grob::preset_extent
	\property Lyrics . LyricText \set #'extent-X = #'(-10.0 . 10.0)
 bar baz
	}
    \paper { linewidth = -1.0 }
}
    
