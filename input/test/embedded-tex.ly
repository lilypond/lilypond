
\version "2.4.0"
\header{ texidoc="@cindex Embedded Tex
You can embed Tex commands in your score. "}

\layout { raggedright = ##t} 


\layout {
    raggedright = ##t
    inputencoding = "TeX"
}
\relative c {
    a''^"$\\int_0^\infty e^{-x^2} dx$" 
}
