\header{
filename =	 "angles.ly";
title =	 "Angels We Have Heard on High";
description =	 "Christmas Carol.";

composer =	 "French Carol, 1854";
enteredby =	 "Jeffrey B. Reed";
copyright =	 "public domain";
}

global = \notes {
	\time 2/2;
	\key f \major;
	\tempo 2=60;
	\clef treble;
}

\version "1.3.93";
% \include "paper16.ly"

flute1 = \notes \relative c'' \context Voice = flute {
	\property Staff.instrument = "flute"

	a'4 a a c   | 
	c4. bes8 a2 |
	a4 g a c    |

	a4. g8 f2   |
	a4 a a c    |
	c4. bes8 a2 |

	a4 g a c                |
	a4. g8 f2               |
	c'2( [ d8 c8 bes8  a8 ] |

	bes2 [ c8 bes8 a8 g8 ]  |
	a2 [ bes8 a8 g8 f8 ]    | 
	)g4. c,8 c2             |

	f4 g a bes     |
	a2 g2          |
	c2( [ d8 c8 bes8  a8 ] |

	bes2 [ c8 bes8 a8 g8 ]  |
	a2 [ bes8 a8 g8 f8 ]    | 
	)g4. c,8 c2             |

	f4 g a bes     |
	a2( )g2        |
	f1            \bar "|."; 
}

flute2 = \notes \relative c'' \context Voice = flute {
	\property Staff.instrument = "flute"
	
	f4 f e e  | 
	g4 e f2   |
	f4 e f f  |

	f4 e f2   |
	f4 f e e  |
	f4 g f2   |

	f4 e f f  |
	f4 e f2   |
	f4 ( [a8 g8 ] f2( |

	)f4 [g8 f8] e2(  |
	)e4 [f8 e8] d2   |
	)c4.c8 c2        |

	c4 e f f  |
	f2 e2     |
	f4 ( [a8 g8 ] f2( |

	)f4 [g8 f8] e2(  |
	)e4 [f8 e8] d2   |
	)c4.c8 c2        |

	c4 e f f  |
	f2( )e2   | 
	c1        |
}

flute1_staff = \context Staff = flute1_group <
	\global
	\flute1
>

flute2_staff = \context Staff = flute2_group <
	\global
	\flute2
>

flutes = \context StaffGroup <
	\flute1_staff
	\flute2_staff
>
	

\score{
        <
       	   \flutes
	>
	\paper{}
	\midi{ 
		\tempo 2 = 60 ;
	}
}
