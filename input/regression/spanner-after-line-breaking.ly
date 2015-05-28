\version "2.19.21"

\header {
texidoc = "The visibility of left-broken line spanners and hairpins
which end on the first note (i.e., span no time between bounds) is
controlled by the callback @code{ly:spanner::kill-zero-spanned-time}.
"
}

\paper { ragged-right = ##t }

\relative {
  \override TextSpanner.bound-details =
    #'((left
        (Y . 0)
        (padding . 0.25)
        (attach-dir . -1)
        (text . "L"))
       (right
        (Y . 0)
        (padding . 0.25)
        (text . "R"))
       (left-broken
        (padding . 5)
        (text . #f))
       (right-broken
        (text . #f)))
  c'1\startTextSpan\< \break
  \override Hairpin.to-barline = ##f
  \override Hairpin.after-line-breaking = ##f
  c2\stopTextSpan\!
  \override TextSpanner.after-line-breaking =
    #ly:spanner::kill-zero-spanned-time
  c\startTextSpan\< \break
  c1\!\stopTextSpan
}
