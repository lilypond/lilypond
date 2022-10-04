\version "2.23.14"

\header {
  lsrtags = "rhythms"

  texidoc = "
The addition of a final @code{mark} can result in the loss of a final
tuplet marking.  This can be overcome by setting
@code{TupletBracket.full-length-to-extent} to @code{false}.
"

  doctitle = "Preventing final mark from removing final tuplet"
}

% due to issue 2362 a long mark such as
%   \textEndMark "Composed Feb 2007 - Feb 2008"
% cannot be used here.

\new Staff {
   \set tupletFullLength = ##t
   \time 1/8
   \tuplet 3/2 8 { c'16 c' c' c' c' c' c' c' c' }
   \tweak direction #DOWN \textEndMark "1234"
}

\new Staff {
  \set tupletFullLength = ##t
  \override TupletBracket.full-length-to-extent = ##f

  \time 1/8
   \tuplet 3/2 8 { c'16 c' c' c' c' c' c' c' c' }
   \tweak direction #DOWN \textEndMark "1234"
}
