\version "2.25.24"

\header {

  texidoc = "References to pages with alignment."

}

#(set-default-paper-size "a8")

\markup { \box \page-ref #'foo "???" "?" " right aligned (default)" }
\markup { \box \override #`(x-align . ,CENTER) \page-ref #'foo "???" "?" " center aligned" }
\markup { \box \override #`(x-align . ,LEFT) \page-ref #'foo "???" "?" " left aligned" }

\markup { \box \override #'(x-align . -2.5) \page-ref #'foo "???" "?" " left outside" }
\markup { \box \override #'(x-align . 0.5) \page-ref #'foo "???" "?" " more right" }
\markup { \box \override #'(x-align . 2.5) \page-ref #'foo "???" "?" " right outside" }

\markup { \box \page-ref #'foo "???" "????" " right aligned (default)" }
\markup { \box \override #`(x-align . ,CENTER) \page-ref #'foo "???" "????" " center aligned" }
\markup { \box \override #`(x-align . ,LEFT) \page-ref #'foo "???" "????" " left aligned" }
