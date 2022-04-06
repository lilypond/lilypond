\version "2.23.8"

\header {
  texidoc = "Header fields can contain @code{\\fromproperty #'header:xxx} markups.
They are correctly converted to strings in PDF metadata.

Warning: the current regression testing infrastructure will not notice
if this test breaks."
  myProp = OK
  title = \markup { This should end in "\"OK\"": \fromproperty #'header:myProp }
}

{ 1 }
