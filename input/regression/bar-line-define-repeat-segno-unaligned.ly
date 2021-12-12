\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="User-defined bar lines with in-staff segni can be printed
by setting the @code{segnoBarType}, @code{startRepeatSegnoBarType},
@code{endRepeatSegnoBarType}, and @code{doubleRepeatSegnoBarType}
context properties, and when the bar line does not fall on a measure
boundary, the value of @code{underlyingRepeatBarType} is appended.
The output should show two adjacent repeated sections with unusually
ornate bar lines with in-staff segni, followed by an in-staff segno
that is flanked by thick bar lines."
}

\layout {
  ragged-right = ##t
}

%% Notice that we define *only* the -|| versions of these.
%%
%% For a score with span-bars, ":;|]." would need to be defined as
%% well, but we don't need to complicate this test with that.
\defineBarLine "S.[|;:-||" #'("." "S.[|;:" "   |") % start repeat
\defineBarLine ":;|].S-||" #'(":;|]." "S" "  |   ") % end repeat
\defineBarLine ":;|]S[|;:-||" #'(":;|]." "S.[|;:" "  |   |") % double repeat
\defineBarLine ".S.-||" #'("." ".S." " = ") % segno alone

\new Score \with {
  %% N.B. We don't want the -|| annotation here.  LilyPond should
  %% append it automatically for unaligned repeats/segni.
  doubleRepeatSegnoBarType = ":;|]S[|;:"
  endRepeatSegnoBarType = ":;|].S"
  startRepeatSegnoBarType = "S.[|;:"
  segnoBarType = ".S."
  %% Omit these to sharpen the focus on the bar lines.
  \omit SegnoMark
} \new Staff \fixed c' {
  r2.
  \inStaffSegno
  \repeat volta 2 { r4 | r2. }
  \inStaffSegno
  \repeat volta 2 { r4 | r2. }
  \inStaffSegno
  r4 | r2. \inStaffSegno r4 |
}
