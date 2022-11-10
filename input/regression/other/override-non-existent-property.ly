\version "2.25.0"

\header {
  texidoc = "@code{\\override} warns and rejects the assignment
if the property does not exist."
}

#(ly:set-option 'check-internal-types #f)
#(ly:set-option 'warning-as-error)

#(for-each
  (lambda (_)
    (ly:expect-warning
     (ly:translate-cpp-warning-scheme
      "the property '%s' does not exist (perhaps a typing error)")
     "nonexistent-property"))
  (iota 2))

{
  \override NoteHead.nonexistent-property = 42
  c'
  % We used not to check the property in this case.
  \override NoteHead.nonexistent-property = #(lambda (grob) 42)
  c'
}
