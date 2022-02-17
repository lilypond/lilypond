\version "2.23.7"

\header {
  texidoc = "The skylines of side-positioned objects can be removed, without
causing crashes."
}

{
  c'\tweak vertical-skylines ##f ^"foo" ^"bar"
}
