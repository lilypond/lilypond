\version "2.25.21"

\header {
  texidoc = "@code{ly:context-set-property!} warns if the property does not
exist."
}

#(ly:set-option 'warning-as-error)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "the property '%s' does not exist (perhaps a typing error)")
  "nonexistentProperty")

{
  \applyContext #(lambda (ctx)
                  (ly:context-set-property! ctx 'nonexistentProperty 42))
  c'
}
