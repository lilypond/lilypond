
\header {
texidoc = "Disregarding the value of @code{systemStartDelimiter}, setting
@code{SystemStart@var{Grob}} style of @code{StaffGroup} to @code{'brace} always
prints a @code{SystemStartBrace}.

Every @code{StaffGroup} should start with a @code{SystemStartBrace}.
"
}

\version "2.21.1"

\layout {
  \context {
    \StaffGroup
    \override SystemStartBar.style = #'brace
    \override SystemStartBrace.style = #'brace
    \override SystemStartBracket.style = #'brace
    \override SystemStartSquare.style = #'brace
  }
}

mus = << b1 b1 >>
<<
\new StaffGroup \with { systemStartDelimiter = #'SystemStartBracket } \mus
\new StaffGroup \with { systemStartDelimiter = #'SystemStartBrace } \mus
\new StaffGroup \with { systemStartDelimiter = #'SystemStartSquare } \mus
\new StaffGroup \with { systemStartDelimiter = #'SystemStartBar } \mus
>>