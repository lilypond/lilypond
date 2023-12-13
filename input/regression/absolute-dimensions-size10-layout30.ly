\version "2.25.12"

\header {
  texidoc = "@code{to-staff-space} produces identical output for different
settings of @code{set-global-staff-size} and @code{layout-set-staff-size}.

For the tests we use @code{\\abs-vspace}, which in turn calls
@code{to-staff-space}."
}

test = \markup \column {
  \abs-fontsize #20 top
  \abs-vspace #50
  \abs-fontsize #20 bottom
}

#(set-global-staff-size 10)

\score {
  { c'1^\markup \typewriter "#(set-global-staff-size 10)"
       _\test }
}

\markup \vspace #2

\score {
  { c'1^\markup \typewriter "#(layout-set-staff-size 30)"
       _\test }

  \layout {
    #(layout-set-staff-size 30)
  }
}
