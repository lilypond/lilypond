\header {


  texidoc = "In chords, ties keep closer to the note head vertically,
but never collide with heads or stems. Seconds are formatted up/down;
the rest of the ties are positioned according to their vertical
position.

The code does not handle all cases. Sometimes ties will printed on top
of or very close to each other. This happens in the last chords of
each system.  "
  

}

\version "2.7.7"

\paper {
  indent = #0.0
  raggedright = ##t
}

testShort =
{
   \time 4/4
  \key c \major
  \relative c'' {
				%  c ~ c
    <c e> ~ <c e>
    <b c e> ~ <b c e>
    <a c e> ~ <a c e>
    <a b e> ~ <a b e>
    <a b e f> ~ <a b e f> 
  }

  \relative c' {
    <c e f a> ~ <c e f a>
    <c e g a> ~ <c e g a>
    
    <a' c d f> ~ <a c d f>  
    <a c e f> ~ <a c e f>
    <a b c d e> ~ <a b c d e>
  }
}  

testLong =
{
  \time 5/8
  \key c \major
  \relative c'' {
    <c e>2 ~ <c e>8
    <b c e>2 ~ <b c e>8
    <a c e>2 ~ <a c e>8
    <a b e>2 ~ <a b e>8
    <a b e f>2 ~ <a b e f>8
  }

  \relative c' {
    <c e f a>2 ~ <c e f a>8
    <c e g a>2 ~ <c e g a>8
    <a' c d f>2  ~   <a c d f>8  
    <a c e f>2  ~   <a c e f>8  
  }
}  

\new Voice
{ \testShort \break
  \transpose c d \testShort \break
  \testLong \break
  \transpose c d \testLong \break
}

