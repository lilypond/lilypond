\version "2.1.22"
\header {


    texidoc = "In extreme cases, you can resort to setting slur
    control-points manually. This involves a lot of trial and error,
    though. Be sure to force line breaks at both sides, since
    different horizontal spacing will require rearrangement of the
    slur."
 
    }


\score {\notes \new PianoStaff  <<
    \context Staff = up { \clef bass s1 * 6 } 
    \context Staff = down \relative c {
	\clef bass
	r4 r8
	\once\override Slur  #'extra-offset = #'(0 . -8)
	\once\override Slur  #'control-points =
	#'((0 . -4) (2 . 0) (60 . 0) (63 . 4))
			   c8( as' f c' as f c as' f
			   \change Staff = up
			   \clef treble
			   c' as f' c as' f c' as
			   f' c as' f c'4)
			   }>>
\paper { raggedright = ##t }
    }
