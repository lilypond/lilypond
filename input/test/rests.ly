\version "1.5.68"
\header {
    texidoc = "rests in various styles."
}

% FIXME: Currently, this file produces "warning: flag `d-3' not found"
% errors (and similar for "d7") from Stem::flag().  This is should not
% happen, since there are no notes/stems in this example.

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
	    %%%% FIXME: The following looks good, but produces
	    %%%% lots of warnings:
	    % \remove Bar_engraver
	}
	\translator {
	    \ScoreContext
	    \remove Bar_number_engraver
	}
    }
}
