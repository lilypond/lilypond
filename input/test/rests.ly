\version "1.5.68"
\header {
    texidoc = "rests in various styles."
}

\score { 
    \context Staff \notes\relative c {
	\property Staff.Rest \set #'style = #'mensural
	r\maxima^"Rest style = \#'mensural"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128 
	\break

	\property Staff.Rest \set #'style = #'neo_mensural
	r\maxima^"Rest style = \#'neo\\_mensural"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128 
	\break

	\property Staff.Rest \set #'style = #'classical
	r\maxima^"Rest style = \#'classical"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128 
	\break

	\property Staff.Rest \set #'style = #'default
	r\maxima^"Rest style = \#'default"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128 
	\break
    }
    \paper {
	\translator {
	    \StaffContext
	    %%%% The following looks good, but produces
	    %%%% lots of warnings:
	    % \remove Bar_engraver
	}
	\translator {
	    \ScoreContext
	    \remove Bar_number_engraver
	}
    }
}
