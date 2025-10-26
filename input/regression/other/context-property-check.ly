\version "2.25.31"

\header {
  texidoc="This test covers @code{\\contextPropertyCheck}.  It should run with
expected warnings.  The visual output is not important."
}

#(ly:set-option 'warning-as-error #t)

{
  %% Warning about Score.timing after failing to access Global is less than
  %% ideal, but users are unlikely to try using Global.
  #(ly:expect-warning
    (ly:translate-cpp-warning-scheme "cannot find context: %s")
    "Global")
  #(ly:expect-warning
    (G_ "~a.~a is ~a; expected ~a")
    "Score" "timing" #t "dummy A")
  \contextPropertyCheck Global.timing "dummy A"

  #(ly:expect-warning
    (G_ "~a.~a is ~a; expected ~a")
    "Score" "timing" #t "dummy B")
  \contextPropertyCheck timing "dummy B"

  s
}

\context VaticanaScore {
  \contextPropertyCheck Score.timing ##f
  \contextPropertyCheck Score.vocalName \default

  #(ly:expect-warning
    (ly:translate-cpp-warning-scheme "cannot find context: %s")
    "VaticanaDrumVoice")
  \contextPropertyCheck VaticanaDrumVoice.timing ##f

  \context VaticanaStaff {
    \set VaticanaScore.measureBarType = "-span|"
    \contextPropertyCheck Score.measureBarType "-span|"
    #(ly:expect-warning
      (G_ "~a.~a is ~a; expected ~a")
      "VaticanaScore" "measureBarType" "-span|" "dummy D")
    \contextPropertyCheck Score.measureBarType "dummy D"

    \contextPropertyCheck Staff.measureBarType #'()
    #(ly:expect-warning
      (G_ "~a.~a is ~a; expected ~a")
      "VaticanaStaff" "measureBarType" "()" "dummy E")
    \contextPropertyCheck Staff.measureBarType "dummy E"

    \unset VaticanaStaff.measureBarType
    \contextPropertyCheck Staff.measureBarType \default
    #(ly:expect-warning
      (G_ "~a.~a is ~a; expected ~a")
      "VaticanaStaff" "measureBarType" (G_ "unset") "dummy F")
    \contextPropertyCheck Staff.measureBarType "dummy F"

    s % descends to Bottom (VaticanaVoice)

    \contextPropertyCheck timing \default
    \contextPropertyCheck Bottom.timing \default
    \contextPropertyCheck VaticanaVoice.timing \default
    \contextPropertyCheck Voice.timing \default

    \contextPropertyCheck autoBeaming ##f
    \contextPropertyCheck Bottom.autoBeaming ##f
    \contextPropertyCheck VaticanaVoice.autoBeaming ##f
    \contextPropertyCheck Voice.autoBeaming ##f

    #(ly:expect-warning
      (G_ "~a.~a is ~a; expected ~a")
      "VaticanaVoice" "autoBeaming" "#f" "dummy G")
    \contextPropertyCheck Voice.autoBeaming "dummy G"

    #(ly:expect-warning
      (ly:translate-cpp-warning-scheme "cannot find context: %s")
      "DrumVoice")
    \contextPropertyCheck DrumVoice.autoBeaming ##f

    \set measureBarType = "dummy H"
    #(ly:expect-warning
      (G_ "~a.~a is ~a; expected ~a")
      "VaticanaVoice" "measureBarType" "dummy H" "#<unspecified>")
    \contextPropertyCheck measureBarType #*unspecified*

    %% It's a bit weird that a string property is allowed to be set to
    %% *unspecified*, but since it is, we want \contextPropertyCheck to be
    %% able to handle it.
    \set measureBarType = #*unspecified*
    \contextPropertyCheck measureBarType #*unspecified*
  }
}

testModImplicit = \with {
  \propertySet instrumentName "dummy L"
  \contextPropertyCheck instrumentName "dummy L"
  \contextPropertyCheck instrumentName "dummy M"
}

#(ly:expect-warning
  (G_ "~a.~a is ~a; expected ~a")
  "Score" "instrumentName" "dummy L" "dummy M")
\new Score \with { \testModImplicit } <<
  R1
>>

#(ly:expect-warning
  (G_ "~a.~a is ~a; expected ~a")
  "Staff" "instrumentName" "dummy L" "dummy M")
\new Score <<
  \new Staff \with { \testModImplicit } <<
    R1
  >>
>>

testModExplicit = \with {
  \propertySet Staff.instrumentName "dummy J"
  \contextPropertyCheck Staff.instrumentName "dummy J"
  \contextPropertyCheck Staff.instrumentName "dummy K"
}

#(ly:expect-warning
  (G_ "~a.~a is ~a; expected ~a")
  "Score" "instrumentName" "dummy J" "dummy K")
\new Score \with { \testModExplicit } <<
  R1
>>

#(ly:expect-warning
  (G_ "~a.~a is ~a; expected ~a")
  "Staff" "instrumentName" "dummy J" "dummy K")
\new Score <<
  \new Staff \with { \testModExplicit } <<
    R1
  >>
>>
