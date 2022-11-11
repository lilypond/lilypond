\version "2.23.82"

\header {
  texidoc = "Text replacements can replace patterns containing
non-ASCII characters.  In particular, this test should also
work if compiled under a non-Unicode-aware locale (e.g.,
@command{LC_ALL=C lilypond @dots{}})."
}

\markup \replace #'(("3è" . "troisième")) { La 3è fois }
