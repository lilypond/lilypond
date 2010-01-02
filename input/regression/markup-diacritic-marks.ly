\version "2.12.3"

\header {
  texidoc = "
Diacritic marks are rendered and positioned correctly.  The
diacritic on line 1 looks like a lower-underline and is centered
beneath the main character.  The diacritic on line 2 is positioned
to the left of the main character, with a tiny space of
separation.  The diacritic on line 3 is positioned directly above
the main character, either centered or shifted slightly to the
left.
"
}

\markup {
  \column {
    כַ וּ וֹ
  }
}
