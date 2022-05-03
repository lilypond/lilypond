\version "2.23.10"

\header {
  texidoc = "@code{\\break} forces a break, even in circumstances where
LilyPond would normally not allow a break."
}

\paper {
  ragged-right = ##t
}

% Break bypasses absence of bar line
{ c'2 \break 2 }

% Break bypasses unbreakable spanner
{ c'2.. 8[ \break 8] 2.. }

% Break bypasses percent repeat
\after 1*3 \break \repeat percent 2 { c'1 1 }
