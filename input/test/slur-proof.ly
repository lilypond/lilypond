
x = {
\outputproperty #(make-type-checker 'note-head-interface) #'extra-offset = #'(-1 . 0)
}

\score{
\context Staff \notes\relative c<
\context Voice=va { \x f()f }
\context Voice=vb { \x g(s4)g }
\context Voice=vc { \x a(s4*2)a }
\context Voice=vd { b(s4*3)b }
\context Voice=ve { c(s4*4)c }
\context Voice=vf { d(s4*5)d }
\context Voice=vg { e(s4*6)e }
\context Voice=vh { f(s4*7)f }
\context Voice=vi { g(s4*8)g }
\context Voice=vj { a(s4*9)a }
\context Voice=vk { b(s4*10)b }
\context Voice=vl { c(s4*11)c }
\context Voice=vm { d(s4*12)d }
\context Voice=vn { e(s4*13)e }
\context Voice=vo { f(s4*14)f }
\context Voice=vp { g(s4*15)g }
\context Voice=vq { a(s4*16)a }
\context Voice=vr { b(s4*17)b }
\context Voice=vs { c(s4*18)c }
\context Voice=vt { d(s4*19)d }
\context Voice=vu { e(s4*20)e }
\context Voice=vv { f(s4*21)f }
\context Voice=vw { g(s4*22)g }
\context Voice=vx { a(s4*23)a }
\context Voice=vy { b(s4*24)b }
\context Voice=vz { c(s4*25)c }
\context Voice=wa { d(s4*26)d }
>
\paper{
indent=0.0\mm;
linewidth=-1.0\mm;
\translator{
\VoiceContext
Slur \override #'direction = #1
Stem \override #'direction = #-1
}
}
}
