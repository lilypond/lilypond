\version "2.25.2"

#(ly:set-option 'warning-as-error)

\header {
  texidoc = "A user-defined bar line with annotated @var{bar-type} does not warn when @var{span-bar} is @code{#t}
or a string literal that is equal to @var{bar-type} (including the annotation).

All bar lines should look the same."
}

\defineBarLine ".;|" #'(#t #t #t)
\defineBarLine ".;|-a" #'(#t #t #t)
\defineBarLine ".;|-b" #'(#t #t ".;|-b")
\defineBarLine ".;|-c" #'(#t #t ".;|")


top = {
  \bar ".;|"
  a'1
  \bar ".;|"
  b'1
  \bar ".;|"
  c''1
  \bar ".;|"
  d''1
  \bar ".;|"
}

top-a = {
  \bar ".;|-a"
  a'1
  \bar ".;|-a"
  b'1
  \bar ".;|-b"
  c''1
  \bar ".;|-c"
  d''1
  \bar ".;|-a"
}

bot = {
  a'1
  b'1
  c''1
  d''1
}


\new StaffGroup
<<
  \top
  \bot
>>

\new StaffGroup
<<
  \top-a
  \bot
>>

