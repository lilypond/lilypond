\version "2.25.1"

\header {
  texidoc = "A user-defined bar line with span bar @code{#f} does not disturb
printing of compound bar lines.  Using neither a string nor a boolean prints
warnings, though does not abort.  An empty string is supported as well.

The first three bar lines should look identical (no span bar)."
}

\defineBarLine ".;|-a" #'(#t #t #f)
\defineBarLine ".;|-b" #'(#t #t "")
\defineBarLine ".;|-c" #'(#t #t foo)
\defineBarLine ".;|" #'(#t #t #t)

#(for-each
  (lambda (i)
    (ly:expect-warning "Setting for span bar needs to be a boolean or string"))
  (iota 6))

mus = {
  \bar ".;|-a"
  a'1
  \bar ".;|-b"
  b'1
  \bar ".;|-c"
  c''1
  \bar ".;|"
  d''
  \bar "|."
}

\new StaffGroup
  <<
    \mus
    \new Dynamics {
      \override Score.TextMark.font-family = #'typewriter
      \textMark "#f"
      s1
      \textMark
      "\"\""
      s1
      \textMark
      "'foo"
      s1
      \textMark "#t"
      s1
    }
    \mus
  >>
