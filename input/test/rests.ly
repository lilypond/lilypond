
\version "1.9.1"
\header {
    texidoc = "@cindex Rests

Rests in various styles.

"
}

% FIXME: Currently, this file produces "warning: flag `d-3' not found"
% errors (and similar for "d7") from Stem::flag().  This is should not
% happen, since there are no notes/stems in this example.

\score {
    \context Staff \notes\relative c {
	\property Score.timing = ##f
	\property Staff.Rest \set #'style = #'mensural
	r\maxima^"Rest style = \#'mensural"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
	\bar empty \break

	\property Staff.Rest \set #'style = #'neo_mensural
	r\maxima^"Rest style = \#'neo\\_mensural"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
	\bar empty \break

	\property Staff.Rest \set #'style = #'classical
	r\maxima^"Rest style = \#'classical"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
	\bar empty \break

	\property Staff.Rest \set #'style = #'default
	r\maxima^"Rest style = \#'default"
	r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128
	\break
    }
    \paper {
	indent = 0.0
	raggedright = ##t
    }
}

