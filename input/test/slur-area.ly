\version "1.3.146"

\score{
\notes\relative c''{

\slurUp
\stemDown
a(a' a,)a
a(a a')a,
a(d a)a
a(a d)a

e'8(e e e e e e)e
a,8(d a a a a a)a
a(a a a a a d)a

c,(g' c e f d b)g 
c,(g' c e f d b)g 

\property Voice.noAutoBeaming = ##t
\stemUp
a8(a a a a a a)a

}
\paper{
indent=0.0\mm
}
}
