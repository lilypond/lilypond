\header {

  texidoc = "Melisma's may be entered manually by substituting
  @code{_} for lyrics on notes that are part of the melisma."

}

\version "2.12.0"
\paper {
  ragged-right = ##t
}

\relative {
  \set melismaBusyProperties = #'()
  c d( e) f f( e) e e  }
\addlyrics
 { Ky -- _ _ ri __ _ _ _  e }
