
\version "2.3.16"
\header {
    
    texidoc = "The area underneath an (up) slur is minimised to improve the shape. "

}

\score{
\relative c''{

\slurUp
\stemDown
a(a' a, a)
a(a a' a,)
a(d a a)
a(a d a)

e'8(e e e e e e e)
a,8(d a a a a a a)
a(a a a a a d a)

c,(g' c e f d b g) 
c,(g' c e f d b g) 

\set autoBeaming = ##f
\stemUp
a8(a a a a a a a)

}
\paper{
indent=0.0\mm
}
}

