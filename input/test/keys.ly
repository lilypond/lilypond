\version "1.3.42";

%{
test key itemv breaking
%}
\score {
  \notes \relative c''
  {
    \key bes; c2 \key c \minor;  c2
    \break
    \key bes \major; c1 \key d;\break c1
  }
}
