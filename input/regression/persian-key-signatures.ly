\version "2.23.6"

\header {
  texidoc = "Test Persian key signatures."
}

\include "persian.ly"

Shur = \relative c' { c4 d ek f g a bf }
Shurk = \relative c' { c4 d ek f g ak bf }
Esfahan = \relative c' { f,4 g ak b c d ef }
Msegah = \relative c' { c4 d ek fo g a bf }
Chah = \relative c' { cs4 d ek fs g a bk }
Mahur = \relative c' { c4 d e f g a b }
Delk = \relative c' { f4 g a bf c dk ef }


showKey =
  #(define-music-function
      (key from to music bar)
      (string? ly:pitch? ly:pitch? ly:music? string?)
    #{
      \key $to #(eval-string key)
      <>^\markup { $key
                   \concat { "(" #(note-name->string to) ")" } }
      \transpose $from $to { #music }
      \bar $bar
    #})


{
  \cadenzaOn

  \showKey "shur" d c \Shur "|"
  \showKey "shurk" d d \Shurk "|"
  \showKey "esfahan" c e \Esfahan "|"
  \showKey "mokhalefsegah" g f \Msegah "|"
  \showKey "chahargah" d g \Chah "|"
  \showKey "mahur" c a \Mahur "||"
  \showKey "delkashMahur" f b \Delk "|."
}

\layout {
  \context {
    \Staff
    printKeyCancellation = ##f
    explicitKeySignatureVisibility = ##(#f #t #t)
  }
}
