\header
{
    texidoc = "A faster lyrics line may be set to a melismatic melody by
setting @code{ignoreMelismata}. A tricky aspect is that ignoreMelismata
must be set  a syllable too soon."
    
}


\paper {
    raggedright = ##t
}

\version "2.3.8"

<<
    \relative \context Voice = "lahlah" {
	\set Staff.autoBeaming = ##f 
	c4
	\slurDotted
	f8.[( g16])
	a4
    }
    \new Lyrics \lyricsto "lahlah" {
	more slow -- ly
    }
    \new Lyrics \lyricsto "lahlah" {
	\set ignoreMelismata = ##t % applies to "fas"
	go fas -- ter
	\unset ignoreMelismata
	still
    }
>>    
