
% lines preceded by a percent sign are comments.
\include "paper16.ly"
\score {
    \notes
    \relative c''
        \sequential {
	    \key g \major
	    \time 3/4    

	\repeat "volta" 2 \sequential {
	    d4 g,8 a b c d4 g, g |
	    e'4 c8 d e fis g4 g, g |
	    c4 d8( )c b a( )b4 c8 b a g |
	    a4 [b8 a] [g fis] g2.  |
	}

        b'4 g8 a b g
        a4 d,8 e fis d |
        g4 e8 fis g d cis4 b8 cis a4 |
        a8-. b-. cis-. d-. e-. fis-.
        g4 fis e |
        fis a,  r8 cis8
        d2.-\fermata
        \bar "|."
    }
    \paper {
       linewidth = 10.0 \cm % standard settings are too wide for a book
   }
}
