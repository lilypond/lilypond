\version "2.3.4"

\header {
    texidoc = "@cindex Bagpipe Music
Here's an example of bagpipe music.
"
}

\paper {
    linewidth = 14.0 \cm
    indent = 0.0 \cm
    \context {
	\Staff
	\override TimeSignature #'style = #'C
	\override TimeSignature #'break-visibility = #begin-of-line-visible
    }	
}

taor =  { \grace {
    g32[ d' g e']
}
	    }
grip =   {
    \grace {
	g32[ b g ]
    }
}
thrd =   {
    \grace {
	g32[ d' c'] 
    }
}
birl =   {
    \grace {
	g32[ a g] 
    }
}
gstd =   {
    \grace {
	g'32[ d' g] 
    }
}
lgg =  {
    \grace {
	g32 
    }
}
lag =  {
    \grace {
	a32 
    }
}
fgg =  {
    \grace {
	f32[ g'32] 
    }
}
dblb =   {
    \grace {
	g'32[ b d'] 
    }
}
dblc =   {
    \grace {
	g'32[ c' d'] 
    }
}
dble =   {
    \grace {
	g'32[ e' f'] 
    }
}
dblf =   {
    \grace {
	g'32[ f' g'] 
    }
}
dblg =   {
    \grace {
	g'32[ f'] 
    }
}
dbla =   {
    \grace {
	a'32[ g'] 
    }
}
cg   =  {
    \grace {
	c'32 
    }
}
eg   =  {
    \grace {
	e'32 
    }
}
gg   =  {
    \grace {
	g'32 
    }
}
dg   =  {
    \grace {
	d'32 
    }
}
hag  =  {
    \grace {
	a'32 
    }
}
gefg =   {
    \grace {
	g'32[ e' f'] 
    }
}
efg  =  {
    \grace {
	e'32[ f'] 
    }
}
gdcg =   {
    \grace {
	g'32[ d' c']
    }
}
gcdg =   {
    \grace {
	g'32[ c' d']
    }
}
	
\score {		
      {
	\time 6/8 \partial 4
	\tieUp
	\slurUp
	\transpose a a' {
	    #(add-grace-property 'Voice 'Stem 'length 6)

	    f'4 |
	    \gg f'4 e'8 \thrd d'4. |
	    \eg a4.(a4) d'8 |
	    \gg d'4 f'8 \dble e'4. ( | \noBreak
	    e'8) d'4 \gg d'4 e'8 |

	    \break
	    \time 9/8
	    \dblf f'2.( f'4) d'8 |
	    \time 6/8
	    \dblg g'4 a'8 \gg a'4. |
	    \thrd d'4.( d'4) \eg a8 |
	    \time 9/8
	    \dble e'4 \lag e'8 \gg  e'16[ d'8. e'8] \gg f'4 g'8 |

	    \break
	    \time 6/8
	    \gg f'4 e'8 \thrd d'4. |
	    \eg a4.( a4) d'8 |
	    \dblg g'4 a'8 \gg a'4. |
	    \thrd d'4.( d'4) f'8 |

	    \break
	    \dblg g'4 e'8( e'8) \dblf  f'8.[ e'16] |
	    \thrd d'4.( d'4) \cg d'8 |
	    \gg c'4 e'8 \thrd d'4.( |
	    d'4.) \gdcg d'4.
	}
    }
}
