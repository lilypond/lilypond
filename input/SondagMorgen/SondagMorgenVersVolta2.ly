\include "deutsch.ly"

marks = \lyrics {
    \marks
    ""1
}

ned = \downbow
op = \upbow

global = \notes {
    \global
    \property Score.repeatCommands = #'((volta #f)(volta "2, 3"))
    \bar ":|"
    s1
}

akk = \lyrics {
    \akk
    "H+9$\\sharp$"
}


mel = \notes {
    \mel
    \relative g'  {
	h2 r8 e4 d8 ~ |
    }
}

kormel = \notes {
    \kormel
    \relative g' {
	cisis2 R2
    }
}

sopsax = \notes {
    \sopsax
    \context Voice=sax \relative c'' {
	cisis4 r8 d8 r dis4 e8
    }
}
altsax = \notes {
    \altsax
    \context Voice=sax \relative c'' {
	a4 r8 d8 r dis4 e8
    }
}
tensax = \notes {
    \tensax
    \context Voice=sax \relative c' {
	dis4 r8 d8 r dis4 e8 |
    }
}

piar = \notes {
    \piar
    \relative c' {
	<dis4 fis a h cisis> r8 <d8-\f a' d> r <dis4 ais' dis> <e8 h' e> | 
    }
}
U = \sustainUp
D = \sustainDown
pial = \notes {
    \pial
    h,4 r8 d8 r dis4 e8  |
}
guitar = \notes {
    \guitar
    r4 e8-2 < a-3 d'-4> e-2 < ais-3 dis'-4> e-2 < h-3 e'-4> |
}

cello = \notes {
    \cello
    R1
}

bas = \notes {
    \bas
    h,4 r8 d8 r dis4 e8  |
}

\include "drumpitch-init.ly"
drhi = \notes {
    \drhi
    cymc4. cymc8 ~ cymc cymc4 cymc8 |
}
drlo = \notes {
    \drlo
    bd4 sn8 bd r bd4 bd8 |
}
