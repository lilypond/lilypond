
\version "2.3.8"
\header {
    texidoc = "@cindex Rests

Rests may be used in various styles.

"
}

% FIXME: Currently, this file produces "warning: flag `d-3' not found"
% errors (and similar for "d7") from Stem::flag().  This is should not
% happen, since there are no notes/stems in this example.

\score {
    \context Staff \relative c {
	\set Score.timing = ##f
	\override Staff.Rest  #'style = #'mensural
	r\maxima^"Rest style = \#'mensural"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
	\bar empty \break

	\override Staff.Rest  #'style = #'neo_mensural
	r\maxima^"Rest style = \#'neo\\_mensural"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
	\bar empty \break

	\override Staff.Rest  #'style = #'classical
	r\maxima^"Rest style = \#'classical"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
	\bar empty \break

	\override Staff.Rest  #'style = #'default
	r\maxima^"Rest style = \#'default"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
	\break
    }
    \paper {
	indent = 0.0
	raggedright = ##t
    }
}

