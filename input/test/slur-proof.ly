\version "1.7.18"
% looks pretty, but it's for regression.  -gp

x = {
  \outputproperty #(make-type-checker 'note-head-interface) 
    #'extra-offset = #'(-1 . 0)
}

\score {
    \context Staff \notes\relative c <
    \context Voice=va { \x f(f-) }
    \context Voice=vb { \x g(s4g-) }
    \context Voice=vc { \x a(s4*2a-) }
    \context Voice=vd { \x b(s4*3b-) }
    \context Voice=ve { \x c(s4*4c-) }
    \context Voice=vf { \x d(s4*5d-) }
    \context Voice=vg { \x e(s4*6e-) }
    \context Voice=vh { \x f(s4*7f-) }
    \context Voice=vi { \x g(s4*8g-) }
    \context Voice=vj { \x a(s4*9a-) }
    \context Voice=vk { \x b(s4*10b-) }
    \context Voice=vl { \x c(s4*11c-) }
    \context Voice=vm { \x d(s4*12d-) }
    \context Voice=vn { \x e(s4*13e-) }
    \context Voice=vo { \x f(s4*14f-) }
    \context Voice=vp { \x g(s4*15g-) }
    \context Voice=vq { \x a(s4*16a-) }
    \context Voice=vr { \x b(s4*17b-) }
    \context Voice=vs { \x c(s4*18c-) }
    \context Voice=vt { \x d(s4*19d-) }
    \context Voice=vu { \x e(s4*20e-) }
    \context Voice=vv { \x f(s4*21f-) }
    \context Voice=vw { \x g(s4*22g-) }
    \context Voice=vx { \x a(s4*23a-) }
    \context Voice=vy { \x b(s4*24b-) }
    \context Voice=vz { \x c(s4*25c-) }
    \context Voice=wa { \x d(s4*26d-) }
  >
  \paper {
    indent=0.0\mm
    raggedright = ##t
    \translator{
      \VoiceContext
      Slur \override #'direction = #1
      Stem \override #'direction = #-1
    }
  }
}
%% new-chords-done %%
