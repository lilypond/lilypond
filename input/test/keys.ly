\version "1.3.5";

%{
test key itemv breaking
%}
\score {
  \notes
  {
    \key bes; c1 \key c \minor;  c1
    \break
    \key bes \major; c1 \key d;\break c1
  }
}
