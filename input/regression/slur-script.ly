
\header {

    texidoc = "Slurs avoid collisions with scripts. If you want to
have a different configuration, the scripts must be moved manually."

    }

\version "2.3.7"
\paper { raggedright = ##t }
{
    \once \override Script #'padding = #1.2
    b8-.( b-.
    \once \override Script #'padding = #1.2
    b-.)
    
    b-.( b-. b-.)
    b--( b-- b--)
    b->( b-> b->)
    b-.--( b-.-- b-.--)
    b---.( b---. b---.)
}
