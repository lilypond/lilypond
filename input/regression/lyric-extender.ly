\version "2.3.17"

\header { texidoc= "A LyricExtender may span several notes.  A
LyricExtender does not extend past a rest."}

\paper { raggedright = ##t }
<<
    \relative c''{
	c8( d e f)
	d1
	r2. f4
    }
    \addlyrics { ah2 __ ha4 __ ah4 }
>>




