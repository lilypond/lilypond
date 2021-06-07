\version "2.23.3"

\header {
  texidoc = "A warning is emitted when the context specified in
@code{alignAboveContext} or @code{alignBelowContext} does not exist,
such as when the context having the @code{alignAboveContext} or
@code{alignBelowContext} property is created before the context
that this property refers to."
}

#(ly:set-option 'warning-as-error)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "alignAboveContext not found: %s") "B")

\new StaffGroup <<
  \new Staff = "A" \with { alignAboveContext = "B" } { c''1 }
  \new Staff = "B" { b'1 }
>>
