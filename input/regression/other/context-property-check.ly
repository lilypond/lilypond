\version "2.25.23"

\header {
  texidoc="This test covers @code{\\contextPropertyCheck}.  It should run with
expected warnings.  The visual output is not important."
}

#(ly:set-option 'warning-as-error #t)

{
  #(ly:expect-warning
    (ly:translate-cpp-warning-scheme "cannot find context: %s")
    "Global")
  \contextPropertyCheck Global.timing "dummy A"

  #(ly:expect-warning
    (ly:translate-cpp-warning-scheme "cannot find context: %s")
    "Bottom")
  \contextPropertyCheck timing "dummy B"

  \context VaticanaScore {
    \contextPropertyCheck Score.timing ##f
    \contextPropertyCheck Score.vocalName #*unspecified*

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
      \contextPropertyCheck Staff.measureBarType #*unspecified*
      #(ly:expect-warning
        (G_ "~a.~a is ~a; expected ~a")
        "VaticanaStaff" "measureBarType" "#<unspecified>" "dummy F")
      \contextPropertyCheck Staff.measureBarType "dummy F"

      s % descends to Bottom (VaticanaVoice)

      \contextPropertyCheck timing #*unspecified*
      \contextPropertyCheck Bottom.timing #*unspecified*
      \contextPropertyCheck VaticanaVoice.timing #*unspecified*
      \contextPropertyCheck Voice.timing #*unspecified*

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
    }
  }
}
