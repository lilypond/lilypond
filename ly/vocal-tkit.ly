%\version "2.19.19"

\include "lyrics-tkit.ly"
\include "staff-tkit.ly"

make-one-voice-vocal-staff =
#(define-music-function (parser location name clef)
   (voice-prefix? string?)

   "Make a staff with one voice and lyrics beneath
     name: the default prefix for instrument name and music
     clef: the clef for this staff "

   (if (make-id name "Music")
     #{
       <<
         \make-one-voice-staff #name #clef "Up"
         #(make-simultaneous-music
           (reverse (map
             (lambda (lyrics-postfix)
               #{ \make-one-stanza "Below" #name #name #lyrics-postfix #} )
               lyrics-postfixes)))
       >>
     #}
     (make-music 'SequentialMusic 'void #t)))

make-two-voice-vocal-staff =
#(define-music-function (parser location name clef v1name v2name)
   (voice-prefix? string? voice-prefix? voice-prefix?)

   "Make a vocal staff with two voices and lyrics above and below
      name: the prefix to the staff name
      clef: the clef to use
    v1name: the prefix to the name of voice one
    v2name: the prefix to the name of voice two "

   (define v1music (make-id v1name "Music"))
   (define v2music (make-id v2name "Music"))

   #{
     <<
       \make-two-voice-staff #name #clef #v1name #v2name
       #(if v1music
           (make-simultaneous-music
              (map
               (lambda (lyrics-postfix)
                 #{ \make-one-stanza "Above" #name #v1name #lyrics-postfix #} )
               lyrics-postfixes)))

       #(if v2music
            (make-simultaneous-music
             (reverse (map
              (lambda (lyrics-postfix)
                 #{ \make-one-stanza "Below" #name #v2name #lyrics-postfix #} )
               lyrics-postfixes))))
     >>
   #} )

make-two-vocal-staves-with-stanzas =
#(define-music-function
  (parser location
    upperName upperClef lowerName lowerClef
    v1name v2name v3name v4name verses)
  (voice-prefix? string? voice-prefix? string?
    voice-prefix? voice-prefix? voice-prefix? voice-prefix? list?)

  "Make two two-voice vocal staves with several stanzas between them.
The number of stanzas is determined by the number of populated verse names.
  upperName: the prefix to the upper staff name
  upperClef: the clef to use on the upper staff
  lowerName: the prefix to the lower staff name
  lowerClef: the clef to use on the lower staff
     vxname: the prefix to the name of voice x, x = 1..4
     verses: the list of verse names containing the stanzas"

    #{
        <<
          \make-two-voice-vocal-staff
            #upperName #upperClef #v1name #v2name
          #(make-simultaneous-music
            (map
             (lambda (verse-name)
              #{ \make-one-stanza
                   #upperName #v1name #v2name #verse-name #} )
            verses))
          \make-two-voice-vocal-staff
            #lowerName #lowerClef #v3name #v4name
        >>
      #} )

