\version "2.1.30"
\header {


    texidoc = "In extreme cases, you can resort to setting the 
    @code{control-points} of a slur manually, althout it involves 
    a lot of trial and error. Be sure to force line breaks at both sides, since
    different horizontal spacing will require rearrangement of the
    slur."
 
    }

%% This slur does not look good. Looks like there have not been a line 
%% break at some point, it is then added, but the slur does not break 
%% in the case of a line break. -HJJ

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
