
\version "2.3.8"
\header{ texidoc="@cindex Embedded Tex
You can embed Tex commands in your score. "}

\paper { raggedright = ##t} 


\paper {
    raggedright = ##t
    inputencoding = "TeX"
}
\relative c {
    a''^"$\\int_0^\infty e^{-x^2} dx$" 
}
