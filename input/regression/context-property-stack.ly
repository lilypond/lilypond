\version "2.25.15"

\header {
  texidoc = "@code{\\pushContextProperty} and @code{\\popContextProperty}
are two new commands for manipulating context properties.  The first one
pushes the current value to a stack and sets a new value, while the second
one pops off the value from the stack and uses it to restore the previous
value."
}

#(ly:expect-warning
    (G_ "context property Staff.fontSize not stacked, setting to default"))
#(ly:set-option 'warning-as-error #t)

{
  c'
  \pushContextProperty Staff.fontSize 3
  c'
  \pushContextProperty Staff.fontSize 6
  c'
  \popContextProperty Staff.fontSize
  c'
  \popContextProperty Staff.fontSize
  c'
  \popContextProperty Staff.fontSize % causing the expected warning
}
