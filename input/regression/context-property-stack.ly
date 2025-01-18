\version "2.25.24"

\header {
  texidoc = "@code{\\pushContextProperty} can be used repeatedly.
@code{\\popContextProperty} pops one value from the stack and uses it to set the
property.  The output should show a normal-sized note at the beginning and the
end, a very large note in the middle, and mid-sized notes between them."
}

#(ly:set-option 'warning-as-error #t)

{
  c'
  \pushContextProperty Staff.fontSize
  \set Staff.fontSize = 3
  c'
  \pushContextProperty Staff.fontSize
  \set Staff.fontSize = 6
  c'
  \popContextProperty Staff.fontSize
  c'
  \popContextProperty Staff.fontSize
  c'
}
