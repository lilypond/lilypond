\include "deutsch.ly"

marks = \lyrics {
    \marks
    ""1
}

ned = \downbow
op = \upbow

global = \notes {
    \global
    \property Score.repeatCommands = #'((volta #f)(volta "3"))
    \bar ":|"
    s1*1
}

akk = \lyrics {
    \akk
    "A11"2 "H11"
}


mel = \notes {
    \mel
    \relative c'' {
	r2 r8 e4 d8 ~ |
    }
}

kormel = \notes {
    \kormel
    R1
}

sopsax = \notes {
    \sopsax
    \context Voice=sax \relative c'' {
	a8 h4 d8 r d4 e8 |
    }
}
altsax = \notes {
    \altsax
    \context Voice=sax \relative c'' {
	a8 h4 d8 r d4 e8 |
    }
}
tensax = \notes {
    \tensax
    \context Voice=sax \relative c' {
	a8 h4 d8 r d4 e8 |
    }
}

piar = \notes {
    \piar
    \relative c'' {
	r8 <g,4 a h d e> <a8 h cis e> r8 <d4 a' d> <e8 h' e> |
    }
}
U = \sustainUp
D = \sustainDown
pial = \notes {
    \pial
    a,,8 a, e, <h,,8 h,> r <d,,4 d,> e,8 | 
}
guitar = \notes {
    \guitar
    e8-2 e-2 < a-3 d'-4> e-2 e-2 < a-3 d'-4> e-2 < h-3 e'-4> |
}

cello = \notes {
    \cello
    R1
}

bas = \notes {
    \bas
    a,8 a, r h, r d4 e8~ |
}

\include "drumpitch-init.ly"
drhi = \notes {
    \drhi
    hh8 hh hh hho r cymc4 cymc8 |
}
drlo = \notes {
    \drlo
    bd8 sn bd bd hhp bd4 bd8 |
}
