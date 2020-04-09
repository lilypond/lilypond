\version "2.19.21"

\header {
  texidoc = "
Point-and-click information can be generated only for certain
event types.
"
}

\pointAndClickTypes #'note-event

\relative {
  c'2\f( f)
}