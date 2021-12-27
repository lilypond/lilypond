\version "2.23.1"

\header {
  texidoc = "@code{\\defineBarLine} accepts annotations in the
end-of-line glyph name that can be used to distinguish bar lines that
should close a volta bracket from those that should not.  Bracket 1
should end open and bracket 2 should end closed."
}

\paper { ragged-right = ##t }

%% Use unique names to prove that LilyPond uses the end-of-line glyph
%% name rather than the mid-line glyph name.
\defineBarLine "||-closed-test" #'("||-eol-test" #f "||")
#(allow-volta-hook "||-eol-test")

\new Score <<
  \repeat volta 2 {
    s1
    \alternative {
      \volta 1 { s1 \bar "||" }
      \volta 2 {}
    }
    s1
    \alternative {
      \volta 1 {}
      \volta 2 { s1 \bar "||-closed-test" }
    }
    s1
  }
>>
