\version "2.17.23"

\header {

  texidoc = "Text is framed properly with @code{\\box},
@code{\\circle}, @code{\\oval} and @code{\\ellipse}"

}

\markup \column {
	\line { \box { text in boxes "1" "12" "123" } }
	\line { \circle { text in circles "1" "12" "123" } }
	\line { \oval { text in ovals "1" "12" "123" } }
	\line { \ellipse { text in ellipses "1" "12" "123" } }
}
