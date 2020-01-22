\version "2.21.0"

\header {
  texidoc = "If the descend-to-context function cannot find or create
its context below the current context, then it does not create its
context anywhere, and it leaves the current context unchanged.

The expected output of this test is one staff with two notes."
}

\new Staff <<
  \new Voice <<
    \absolute f''
    %% can't create a Staff below a Voice
    #(descend-to-context
      #{ \absolute d'' #}
      'Staff "fail"
      #{ \with { instrumentName = "FAIL" } #} )
  >>
>>
