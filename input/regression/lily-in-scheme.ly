\header {
    texidoc = "LilyPond syntax can be used inside scheme to
build music expressions, with the @code{#@{ ... #@}} syntax.
Scheme forms can be introduced inside these blocks by escaping 
them with a @code{$}, both in a LilyPond context (see the @code{music} variable) or in a Scheme 
context (see the @code{$padding} and @code{$(* padding 2)} forms.)"
}
\version "2.3.4"
\paper { raggedright = ##t }

withPaddingA = #(def-music-function (location padding music) (number? ly:music?)
                   #{ \override TextScript #'padding = #$padding
                      $music 
                      \revert TextScript #'padding #})
  
withPaddingB = #(def-music-function (location padding music) (number? ly:music?)
                   #{ \override TextScript #'padding = #$(* padding 2)
                      $music 
                      \revert TextScript #'padding #})
  
withPaddingC = #(def-music-function (location padding music) (number? ly:music?)
                   #{ \override TextScript #'padding = #(+ 1 $(* padding 2))
                      $music 
                      \revert TextScript #'padding #})
  
  \score {
      \notes {
          c'^"1"
          \withPaddingA #2
            { c'^"2" c'^"3"}
          c'^"4"
          \withPaddingB #2
            { c'^"5" c'^"6"}
          c'^"7"
          \withPaddingC #2
            { c'^"8" c'^"9"}
          c'^"10"
      }
  }
