\header {

texidoc = "You can switch the Voice context for a lyrics melody
    during a lyrics line using @code{associatedVoice}.  Here, the
    syllables \"rannosau\" are set to triplets.

A tricky aspect is that @code{associatedVoice} needs to be changed one
syllable too soon.

"
}

\paper {
    raggedright = ##t
}

\version "2.3.8"

<<
    \relative \context Voice = "lahlah" {
	\set Staff.autoBeaming = ##f 
	c4
	<<
	    \context Voice = alternative {
		\voiceOne
		\times 2/3 {

		    % show associations clearly.
		    \override NoteColumn #'force-hshift = #-3
		    f8 f g
		}
	    }
	    {
		\voiceTwo
		f8.[ g16]
		\oneVoice
	  } >>
	a8( b) c

    }
    \new Lyrics \lyricsto "lahlah" \lyrics {
	Ju -- ras -- sic Park
    }
    \new Lyrics \lyricsto "lahlah" \lyrics {

	% Tricky: need to set associatedVoice
	% one syllable too soon! 
	\set associatedVoice = alternative % applies to "ran"
	Ty --
	ran  --
	no --
	\set associatedVoice = lahlah % applies to "rus"
	sau -- rus Rex
    } >>
    


