\include "paper20.ly"
\version "1.4.7"
\header {
  title = "Rondo Alla Turca"
  subtitle = "Turkish March"
  composer = "W. A. Mozart"
  piece = "\\quad \\quad \\quad \\quad \\quad Allegretto"
  mutopiatitle = "Rondo Alla Turca"
  mutopiacomposer = "W. A. Mozart (1756-1791)"
  mutopiainstrument = "Piano"
  style = "classical"
  copyright = "Public Domain"
  maintainer = "Rune Zedeler"
  maintainerEmail = "rz@daimi.au.dk"
  lastupdated = "2001/sep/15"
  tagline = "\\parbox{\hsize}{\\thefooter\\quad\\small \\\\This music is part of the Mutopia project, \\texttt{http://www.mutopiaproject.org/}\\\\It has been typeset and placed in the public domain by " + \maintainer + " (" + \maintainerEmail + ").\\\\Unrestricted modification and redistribution is permitted and encouraged---copy this music and share it!}"
}

volta = "volta"

\include "deutsch.ly";

#(define (remove-scripts music)
  (let* ((es (ly-get-mus-property music 'elements))
         (e (ly-get-mus-property music 'element))
         (body (ly-get-mus-property music 'body))
         (alts (ly-get-mus-property music 'alternatives)))

    (if (pair? es)
        (begin
	 (ly-set-mus-property
          music 'elements
          (map (lambda (x) (remove-scripts x)) es))
	 (let recurse ((elts (ly-get-mus-property music 'elements)))
	   (if (and (pair? elts) (pair? (cdr elts)))
	     (let ((name (ly-music-name (cadr elts))))
	       (if (or (equal? name "Articulation_req") (equal? name "Text_script_req"))
	         (begin
		   (set-cdr! elts (cddr elts))
		   (recurse elts))
		 (recurse (cdr elts))))))))

    (if (music? alts)
        (ly-set-mus-property
         music 'alternatives
         (remove-scripts alts)))

    (if (music? body)
        (ly-set-mus-property
         music 'body
         (remove-scripts body)))

    (if (music? e)
        (ly-set-mus-property
         music 'element
         (remove-scripts e)))
    music))

righta = \notes \transpose c''' {
  \scriptUp
  \property Staff.Fingering \override #'direction = #1
    \partial 4 h,16-4-\p( a, gis, a,-1 | )c8-3 r d16-4( c h, c-1 | )e8-3 r f16-4( e dis e-1 | h-4 a gis a h a gis a | )c'4-\accent
    a8-.-3 c'-.-5 | \grace {[g!32( )a]} h8-.-5-\sfz <fis-2 a-.-4> <e g-.> <fis a-.> | \grace {[g32( )a]} h8-.-\sfz <fis-2 a-.> <e g-.> <fis a-.> | \grace {[g32( )a]} h8-.-\sfz <fis-2 a-.> <e g-.> <dis fis-.> e4-- 
}
rightaa = \notes \transpose c''' {
    \partial 4 h,16-4-\p( a, gis, a,-1 | )c8-3 r d16-4( c h, c-1 | )e8-3 r f16-4( e dis e-1 | h-4 a gis a h \< a gis a | \! )c'4-\accent
    a8-.-3 h-. | c'-.-\accent \> h-. a-.-1 \! gis-.-2 a-. e-. f-.-4 d-.-2 | c4-- h,8.-2-\trill( a,32 h, | )a,4--
}

rightb = \notes \transpose c''' {
  [<c8-1-\mp e-3-.> <d! f!-.>] | <e-3 g-.-5> <e-1 g-.-3> a16-4( g f )e | \stemUp <d4-\accent-4 \context Voice = another {\stemDown \slurDown h,8-2()g, }> \stemBoth
  <c8-1 e-3-.> <d! f!-.> | <e-3 g-.-5> <e-1 g-.-4> a16-4( g f )e | <h,4-2-\accent d-4-->
  <a,8-1 c-3-.> <h, d> | <c-3 e-.-5> <c-1 e-.-3> f16-4( e d )c | \stemUp <h,4-\accent-4 \context Voice = another {\stemDown \slurDown gis,8-2()e,}> \stemBoth
  <a,8 c-.> <h, d> | <c e-.> <c-1 e-.> f16( e d )c | <gis,4-2-- h,-4-\accent>
}

rightca = \notes \context Voice = voicea \relative c''' { a8-.-\f h-. | cis4-\accent a8-. h-. cis-.-\accent h-. a-. gis-. | fis-. gis-. a-. h-. gis-4( )e-. 
a8-. h-. | cis4-\accent a8-. h cis-.-\accent h-. a-. gis-. | fis-. h-. gis-. e-. a4
}
rightc = \notes < \apply #remove-scripts \rightca \transpose c \rightca >
rightco = \notes \relative c''
{ \stemDown
  a16-\f( a' h, h' | cis,-\accent )cis' r8 a,16( a' h, h' cis, cis' h, h' a, a' gis, )gis' | fis,( fis' gis, gis' a, a' h, h' gis, gis' e, )e' 
  a,16(   a' h, h' | cis,-\accent )cis' r8 a,16( a' h, h' cis, cis' h, h' a, a' gis, )gis' | fis,( fis' h, h' gis, gis' e, e'
}
rightcoa = \notes \context Voice < )a'4 a'' >

rightd = \notes \relative c''' {
  cis16-3-\p( d cis h a h a gis-3 fis-2 a gis fis | eis fis gis eis cis-2 dis eis cis-1 | fis-4 eis-1 fis gis a gis a-1 h | cis his cis his
  cis d cis )h | a( h a gis-3 fis a gis fis | e fis gis e cis-2 dis e cis | dis-3 e fis dis his-1 cis dis his | )cis4
}
righte = \notes \relative c''' {
  e,16-5(-\f d! cis h! | a h cis d-1 e fis gis a | )a-\accent-4( gis fis )e e-5( d cis h | )a-1( h cis d-1 e fis gis a ais8-\accent-3 )h-.-4
  e,16-5( d  cis h  | a h cis d-1 e fis gis a | )a-\accent-4( gis fis )e e-5( d cis h | cis-3 e a,-1 cis-4 h d gis,-2 h | )a4--
  cis'16-3-\p( d cis h a h a gis-3 fis-2 a gis fis | eis fis gis eis cis-2 dis eis )cis-1 | fis-4( \< eis-1 fis gis a gis a-1 \! h | cis his cis his
  cis his cis ais-2 | )d-4( \> cis d cis d cis d cis | d cis  h a gis-2 a h \! gis | a-\p h cis fis,-2 eis fis gis eis )fis4--
}

strum = \notes \transpose c' { <
  \context Voice = strumUp {\stemUp cis'2-\arpeggio-\accent}
  \context Voice = strumDown {\stemDown < cis4-\arpeggio e a> }
>
\stemBoth
}

rightf = \notes \transpose c''' {
  < {\stemUp cis'8. cis'16} \context Voice = another {\stemDown cis4} >
  \strum \strum d'16-4( )cis'-. h-. cis'-. d'( )cis'-. h-. cis' <d'2-\accent a fis>
  \repeat unfold 4 { \grace{d'8( } < )cis'8-. a e> } | < {\stemUp \slurUp h4.-3()e'8-. \stemBoth} \context Voice = another <gis2 e> >
  \strum \strum d'16-4( )cis'-. h-. cis'-. d'( )cis'-. h-. cis' <d'2-\accent a fis> \grace{d'8( } < )cis'2-. a e> 
  \repeat unfold 4 { \grace{cis'8( } < )h8-. gis e> }

  a4-\p-- \grace {[e32()a]} cis'8.-.-4 cis'16 \repeat unfold 2 { \grace {[e32()a]} cis'2-\accent } |
  d'16-4( )cis'-. h-. cis'-. d'( )cis'-. h-. cis' | d'2-\accent | \repeat unfold 4 { \grace{d'8( } ) cis'8-. } h4.-2()e'8-.
  <\strum s2-\f> \strum d'16-4( )cis'-. h-. cis'-. d'( )cis'-. h-. cis' <d'2-\accent a fis> \grace{d'8( } < )cis'2-. a e> 
  \repeat unfold 4 { \grace{cis'8( } < )h8-. gis e> }
  <a,4. cis e a--> <cis8 cis'-.> <a,4. a--> <e8 e'-.> <a,4. a--> <cis8 cis'-.> <a, a-.> <cis8 cis'-.> <a, a-.> <e8 e'-.> <a,4 a-.> 
  <a,4-. cis e a-.( > <) a,4-. cis e a-. > r4
}


lefta = \notes {
  \partial 4 r4 | a8-5( <)c' e'-.> <c' e'-.> <c' e'-.> | a8( <)c' e'-.> <c' e'-.> <c' e'-.> | a8-. <c' e'-.> a8-. <c' e'-.> | a8( <)c' e'-.> <c' e'-.> <c' e'-.> |
  e-. <h e'-.> <h e'-.> <h e'-.> | e-. <h e'-.> <h e'-.> <h e'-.> | e-. <h e'-.> h, h | e4--
}
leftaa = \notes { \partial 4
  r4 | a8-5( <)c' e'-.> <c' e'-.> <c' e'-.> | a8( <)c' e'-.> <c' e'-.> <c' e'-.> | a8-. <c' e'-.> a8-. <c' e'-.> | f8( <)a dis'-.> <a dis'-.> <a dis'-.> |
  e-. <a-. e'>  d!-. <f-. h> c-. <e-. a> d-. <f-. h> <e-. a> <e-. a> <e-. gis> <e-. gis> <a,4 a-->
}
leftb = \notes { \partial 4
  \repeat unfold 2 {r4 | c8-. c'-. e-. e'-. | g4 }
  \repeat unfold 2 {r4 | a,8-. a-. c-. c'-. | e4 }
}

stra = \notes { \grace {[a,32( cis )e]} a8-. a-. }
strd = \notes { \grace {[d,32( fis, )a,]} d8-. d-. }
strdis = \notes { \grace {[dis,32( fis, )a,]} dis8-. dis-. }
stre = \notes { \grace {[e,32( gis, )h,]} e8-. e-. }
stral = \notes { \stra a8-. a-. }
strdl = \notes { \strd d8-. d-. }
strel = \notes { \stre e8-. e-. }

leftc = \notes { \partial 4
  r4 \stral \stral \strd \strdis \strel \stral \stral \strd \stre
}

leftd = \notes \relative c { \partial 4
  r4 | fis8-5( <)a cis-.> <a cis-.> <a cis-.> |
  gis8-4( <)h cis-.> <h cis-.> <h cis-.> |
  fis8( <)a cis-.> <a cis-.> <a cis-.> |
  eis8( <)gis cis-.> <gis cis-.> <gis cis-.> |
  fis8( <)a cis-.> <a cis-.> <a cis-.> |
  gis8( <)cis e-.> <cis e-.> <cis e-.> |
  gis8( <)dis' fis-.> <dis fis-.> <dis fis-.> |
  <cis4 e-->
}

lefte = \notes \relative c' { \partial 4
  r4 | a8( <)cis e-.> <cis e-.> <cis e-.> |
  h-. <d e-.> gis,-. <d' e-.> |
  a8( <)cis e-.> <cis e-.> <cis e-.> |
  e,8( <)gis d'-.> <gis d'-.> <gis d'-.> |
  a8( <)cis e-.> <cis e-.> <cis e-.> |
  h-. <d e-.> gis,-. <d' e-.> |
  a-. fis-. d-. e-. a,-. a'-. r4 |

  fis8( <)a cis-.> <a cis-.> <a cis-.> |
  gis8( <)h cis-.> <h cis-.> <h cis-.> |
  fis8( <)a cis-.> <a cis-.> <a cis-.> |
  cis,( <)gis' cis-.> <g cis-.> <fis cis'-.> |
  h,8( <)fis' h-.> <fis h-.> <fis h-.> |
  h,8( <)gis'! h-.> <gis h-.> <gis h-.> |
  cis,-. <fis a-.> cis-. <gis' h-.> <fis4 a-->
}

leftf = \notes { 
  a8-. a8-. \stral \stral \stral \strdl \stral \strel \stral \stral \stral \strdl \stral \strel
  a16( e' cis' e' a e' cis' )e' \repeat unfold 6 { a16 e' cis' e' } \repeat unfold 2 { a16 fis' d' fis' } \repeat unfold 2 { a16 e' cis' e' } \repeat unfold 2 { e16 e' gis e' }
  \stral \stral \stral \strdl \stral \strel \stral \stral \stral \stra \stra a,4-. <a, cis e a-. ( > < )a, cis e a-. > r4  
}

global = \notes {\time 2/4 }

right = \notes {
      \global \clef G \repeat \volta 2 \righta \repeat \volta 2 {\rightb \rightaa } \key a \major \repeat \volta 2 \rightc
      \repeat \volta 2 \rightd \repeat \volta 2 \righte \repeat \volta 2 \rightc
       \key a \minor \repeat \volta 2 \righta \repeat \volta 2 {\rightb \rightaa } \key a \major \repeat \volta 2 \rightco \alternative { \rightcoa {\partial 4 \rightcoa } }
       \rightf \bar "|."
}

left = \notes {
      \global \clef F \repeat \volta 2  \lefta \repeat \volta 2 { \leftb  \leftaa } \key a \major \repeat \volta 2 { \leftc a,4 }
      \repeat \volta 2 \leftd \repeat \volta 2 \lefte \repeat \volta 2 { \leftc a,4 }
      \key a \minor \repeat \volta 2  \lefta \repeat \volta 2 { \leftb  \leftaa } \key a \major \repeat \volta 2 \leftc \alternative { a,4 {\partial 2 \stra} }
      \leftf \bar "|."
}


\score { \notes
  \context GrandStaff <
    \property GrandStaff.connectArpeggios = ##t
    \context Staff = up {
      \right
    }
    \context Staff = down {
      \property Staff.VoltaBracket = \turnOff
      \left
    }
  >
  \paper {
    \translator {
      \GraceContext
      Slur \override #'direction = #-1
    }
    \translator {
      \ScoreContext
      SpacingSpanner \override #'arithmetic-basicspace = #1.8
      GraceAlignment \override #'horizontal-space = #-0.4
      PaperColumn \override #'before-grace-spacing-factor = #1.0
      
    }
    interscoreline = 6.0 \pt

  }
}
