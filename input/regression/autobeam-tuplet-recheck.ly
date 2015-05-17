\version "2.19.21"

\header {

  texidoc = "Autobeam rechecking works properly with tuplets.
In the example, the first beat should be beamed completely together."

}

\relative {
  \time 2/4
  c'16 c
  \tuplet 3/2 { c8 c16 }
  \tuplet 3/2 { c8 c16 }
  c16 c
}
