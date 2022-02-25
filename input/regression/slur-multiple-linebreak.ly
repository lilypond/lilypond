\version "2.19.21"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "already have slur"))

\header {
  texidoc = "An additional opening slur during a running slur should be ignored
(and a warning printed), but never influence the slur's extents."
}

\paper { ragged-right = ##t }

\relative {
  \key fis \major
  c'1(
  \break
  a2 b4 c)
}

\relative {
  \key fis \major
  c'1(
  \break
  a2( b4 c)
%   ^ extra SlurEvent
}
%% END
