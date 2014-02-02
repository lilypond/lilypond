
\header {

    texidoc = "Slur formatting is based on scoring. A large number of
    slurs are generated. Each esthetic aspect gets demerits, the best
    configuration (with least demerits) wins. This must be tested in
    one big file, since changing one score parameter for one situation
    may affect several other situations.

    Tunable parameters are in @file{scm/@/slur.scm}.
"

}

\version "2.19.2"
\layout {
    ragged-right = ##t
%    #(define debug-slur-scoring #t)
}

\relative c'' {

    \grace e=''16( d8.[) c16]
    d=''8.[ \grace f16( e16)]
    s2
    << {c=''8.([ es16] bes4~4 )}
       \\
       {r8  <as es> r <f des> r }
    >>
    \new Voice { \voiceOne b='8[ c16( d])  }
    g,='8[( a b b! ]  c4  bes)
    bes='8( f' des bes) as4( bes)
    r8 d( f e d c b a)
    cis=''4( d)  f=''16( e)    d( c)
    s4

    c=''2(~c8 d16 c b8 a)

    <c,=' g>4 ( f <g b>) f
    <c g>^( f <g b>) f
    <c g>_( f <g b>)
    <g b>_( g  <b d>)
    <g b>_( \stemDown g \stemNeutral  <b d>)
    c,='^( c'' c)
    c,,^( c'')
    c,,^( c')
    | b='2( a4) s4
    | b='4.( c8) s2
    | << c=''1_(
       { s2 \grace { b16 c } } >>
    \break
    b4)
    e=''4.( c8) s4
    | << { b='8[( c]) } \\
	 { b='8[( c]) }>>


    s2.|
    e4( dis4)
    e4( dis4)
    g,='16( b d fis)
    \clef bass a,,=8[ e16(f] g[ a b d,)]  s4 | \break
    e=8[( f] g[ a b d,)]  s4 |

    \clef treble
    \new Voice {
	\slurDown
	c''=''4(^"slurs forced down"  d, c') s4
	f=''2( d4 f | g c a f | d c f2 | f1) |
    }

%    \override Slur.eccentricity = #-2
    c=''8 ( d[ b f d] a'[ c])
}

