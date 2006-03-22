
\version "2.8.0"

% definitely wil be renamed to something.
%{
(Message vers:9)
To: Adrian Mariano <adrian@camcornelledu>.
cc: gnu-music-discuss@gnu.org
Subject: Re: Switching from one staff to two staves 
Reply-To: janneke@gnu.org
In-reply-to: Your message of "Tue, 19 Jan 1999 12:27:10 EST."
             <199901191727.MAA29757@avalanchecamcornelledu>. 
Date: Wed, 20 Jan 1999 09:39:22 +0100
From: Jan Nieuwenhuizen <jan@beavis-nt>

On Tuesday, 19 January 1999, Adrian Mariano writes:

>> I want to typeset something which starts out with just one staff and then
>> harmony comes in and there are two staves.  I can't figure out how to do
>> this.  I get an extra blank staff during the second section (using
>> Lily 1.1.15):

There used to be an example called 'multi.ly'...
Try this (we'll include it in pl23/24):

extra-staff.ly:
%}


\header{ texidoc="@cindex Staff Extra
@cindex Staff Ossia
You can add (possibly temporarily) an extra staff after the beginning of 
a piece. "
}

\score {
	<<
		\new Staff \relative c''{ c1 c c c c }
		\new StaffGroup \relative c''{ 
			\new Staff 
			c1 c
			<< c1 \new Staff { c1 } >>
			c
		}
	>>
  	\layout {
		ragged-right = ##t
		\context{
			\Score

		}
	}
}




