\version "1.0.7";

%{
test key itemv breaking
%}
\score {
  \notes
  {
    \key bes; c1 \key c;  c1
    \break
    \key bes; c1 \key d;\break c1
  }
}
