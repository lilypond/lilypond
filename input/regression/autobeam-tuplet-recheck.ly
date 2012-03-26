\version "2.15.34"

\header {

  texidoc = "Autobeam rechecking works properly with tuplets.
In the example, the first beat should be beamed completely together."

}

\relative c' {
  \time 2/4
  c16 c
  \times 2/3 { c8 c16 }
  \times 2/3 { c8 c16 }
  c16 c
}
