\version "2.17.23"

\header {

  texidoc = "Text is framed properly with @code{\\box},
@code{\\circle}, @code{\\oval}, and @code{\\ellipse}."

}

\markup \column {
  \line { \box { text in boxes "1" "12" "123" } }
  \line { \circle { text in circles "1" "12" "123" } }
  \line { \oval { text in ovals "1" "12" "123" } }
  \line { \ellipse { text in ellipses "1" "12" "123" } }
}

\markuplist \override #'((padding . 2)
                         (baseline-skip . 15)) \table #'(1 0) {
  "without ‘bbox’ property:"
    \vcenter \circle
      \override #'(baseline-skip . 2.5) \center-column {
        "short" "short" "very very long" }
  "with ‘bbox’ property:"
    \vcenter \override #'(bbox . #t) \circle
      \override #'(baseline-skip . 2.5) \center-column {
       "short" "short" "very very long" } }
