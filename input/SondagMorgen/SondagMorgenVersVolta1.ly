\version "1.5.68"
\include "deutsch.ly"

marks = \lyrics {
    \marks
    ""1
}

ned = \downbow
op = \upbow

global = \notes {
    \global
    \property Score.repeatCommands = #'((volta "1"))
    s1
}

akk = \lyrics {
    \akk
    "D11"1
}


mel = \notes {
    \mel
    \relative g' {
	a2 r8 d,8 [g a]  |
    }
}

kormel = \notes {
    \kormel
    \relative c'' {
	d2 R2
    }
}

sopsax = \notes {
    \sopsax
    \context Voice = sax \relative c'' { a2 r2 }
}
altsax = \notes {
    \altsax
    \context Voice = sax \relative c' {e2 r2 }
}
tensax = \notes {
    \tensax
    \context Voice = sax \relative c'  {c2 r2 }
}

piar = \notes {
    \piar
    \relative c' {
	<c e g a> r8 d g a
    }
}
U = \sustainUp
D = \sustainDown
pial = \notes {
    \pial
    s1*0 \D d1*1/2 \U \D s2 \U |

}
guitar = \notes {
    \guitar
    \relative c' {
	<d,2-2 g-3 h-4 e-5 a-6> r2 |
    }
}

cello = \notes {
    \cello
    R1
}

bas = \notes {
    \bas
    d1 |
}

\include "drumpitch-init.ly"
drhi = \notes {
    \drhi
    cymc4 tommh8 toml sn hho sn sn |
}
drlo = \notes {
    \drlo
    bd4. bd8 bd4 bd4    
}
