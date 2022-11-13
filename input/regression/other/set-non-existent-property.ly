\version "2.25.0"

\header {
  texidoc = "@code{\\set} warns and rejects the assignment
if the property does not exist."
}

#(ly:set-option 'warning-as-error)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' does not exist (perhaps a typing error)")
  "nonexistentProperty")

{
  \set nonexistentProperty = 42
  c'
}
