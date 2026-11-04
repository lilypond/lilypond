\version "2.27.5"

\header {
  texidoc = "@code{\\tweak} warns and rejects the assignment
if the value does not match the property's type predicate."
}

#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' must be of type '%s', ignoring invalid value '%s'")
  "color"
  "color"
  "red")

{ \tweak color #'red c'1 }
