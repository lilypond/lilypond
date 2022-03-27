\version "2.23.5"

\header {
  texidoc = "Music functions can be called directly from Scheme."
}

\new Staff {
  #(relative
    (make-sequential-music
     (list
      (time '(3 2) '(5 . 4)) #{ e'8 8 8 8 8 8 8 8 8 8 #}
      (clef "bass")
      (key #{ a #} major)
      (time '(3 . 4))
      (tuplet '(2 . 3) #{ 4. #} #{ d,8 e fis gis #})
      (key)
      #{ a2. #})))
}
