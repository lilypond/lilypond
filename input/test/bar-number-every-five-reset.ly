\version "1.9.4"

\header {
    texidoc = "@cindex Bar Number Every Fifth Reset
If you would like the bar numbers to appear at regular intervals, but
not starting from measure zero, you can use the context function,
@code{set-bar-number-visibility}, to automatically set
@code{barNumberVisibility} so that the bar numbers appear at regular
intervals, starting from the @code{\applycontext}.
"

}

resetBarnum = \context Score \applycontext
  #(set-bar-number-visibility 4)
\score {
    <<
        \notes \transpose c c'' {
	    \property Score.BarNumber \override #'break-visibility =#end-of-line-invisible
	    \property Score.RehearsalMark \override #'padding = #2.5
	    \mark "A" \resetBarnum
	    \repeat unfold 10 c1
	    \mark \default \resetBarnum
	    \repeat unfold 8 c
            \bar "|."
        }
    >>
\paper{raggedright = ##t}
}
