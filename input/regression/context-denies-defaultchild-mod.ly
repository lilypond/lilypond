\version "2.19.80"

\header {
  texidoc = "@code{\\denies} @var{context} in a @code{\\with} block cancels
a prior @code{\\defaultchild} @var{context}.  CREATED should appear in the
left margin."
}

\layout {
  \context {
    \Score
    \accepts "AdHocStaff"
  }

  \context {
    \Staff
    \name "AdHocStaff"
    \alias Staff
    instrumentName = "CREATED"
  }

  \context {
    \Staff
    instrumentName = "DENIED"
  }

}

\new Score \with {
  %% If this cancelled \accepts without cancelling \defaultchild,
  %% Score would still accept a Staff because default children are
  %% implicitly accepted with the highest priority.
  \denies "Staff"
} <<
  %% This builds a path of accepted contexts from Score to Voice.
  %% Which context it chooses tells whether \denies did its job.
  \context Voice s1
>>
