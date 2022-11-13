\version "2.25.0"

\header {
  texidoc = "@code{\\set} warns and rejects the assignment
if the value does not match the property's type predicate."
}

#(ly:set-option 'warning-as-error)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' must be of type '%s', ignoring invalid value '%s'")
  "doubleSlurs"
  "boolean"
  "42")

{
  \set doubleSlurs = 42
  c'
}
