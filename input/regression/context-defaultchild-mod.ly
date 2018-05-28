\version "2.19.80"

\header {
  texidoc = "@code{\\defaultchild} can be overridden in @code{\\with} blocks.
CREATED should appear in the left margin."
}

\layout {
  \context {
    \Staff
    \name "AdHocStaff"
    \alias Staff
    instrumentName = "CREATED"
  }

  \context {
    \Staff
    instrumentName = "UNUSED"
  }
}

\new Score \with {
  \defaultchild "NoneSuch" % prove that the last wins
  \defaultchild "AdHocStaff"
} <<
  %% This builds a path of accepted contexts from Score to Voice.
  %% Which context it chooses tells whether \defaultchild did its job.
  \context Voice s1
>>
