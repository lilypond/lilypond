
\header {

    texidoc = "Property overrides and reverts from @code{\\grace} do
    not interfere with the overrides and reverts from polyphony."

}

\version "2.3.4"
\score {
   \relative c'' {
      <<
	  { \grace e8 d2 }
	  \\ { a2 } >>
  }
}

