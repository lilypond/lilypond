\version "2.25.21"

\header {
  texidoc = "@code{\unset} warns if the property does not exist."
}

#(ly:set-option 'warning-as-error)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' does not exist (perhaps a typing error)")
  "nonexistentProperty")

{
  \unset nonexistentProperty
  c'
}
