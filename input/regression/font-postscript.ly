\header {
  texidoc = "This file demonstrates how to load different (postscript)
fonts.  The file @file{font.scm} shows how to define the scheme-function
@code{make-default-fonts-tree}."
}

\version "2.16.0"

\paper {
  #(define text-font-defaults
    '((font-encoding . latin1)
      (baseline-skip . 2)
      (word-space . 0.6)))

  #(set! fonts (make-default-fonts-tree 1.0))
}

\layout {
  line-width = 160 \mm - 2.0 * 9.0 \mm

  indent = 0.0\mm
  ragged-right = ##t
}

{
  \key a \major
  \time 6/8
  cis''8.^"test!" d''16 cis''8 e''4 e''8
}
