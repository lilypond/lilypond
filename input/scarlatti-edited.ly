% test.ly LilyPond 1.3.70 -- 6 July 2000

\header {
	title = "Scarlatti test";
	composer = "D. Scarlatti and J. D. S. Sankey";
}

\include "scarlatti-properties.ly"
\include "scarlatti-paper.ly"

forceStaffDown = \translator "Staff" = "down"
forceStaffUp = \translator "Staff" = "up"


\score{
	\notes \context PianoStaff <
	\context Staff=up <
	\key d \major ;
	\time 3/8;
	\clef treble;
	\autochange Staff
	\context Voice=va< \zs
	\su
	\context Thread=ta{ \n
		\rh \sm [ \times 2/3{a'32 d'' fis''}\n a''16] a'8 g'|
		%a2
		\times 2/3{\lh d'16 \rh fis' g' a' b' cis'' d'' fis'' g''}|
		%a3
		a''8 a' g'|
		%a4
		a'8 s \lh d''|
		%a5
		s8 \rh fis'' s|
		%a6
		a''32 s fis'' s d'' s a' s d' s a s|
		%a7
		\forceStaffUp r16 a' fis'8. d'16~
		%a8
		d'16 g' e'8. cis'16|
		%a9
		r16 a' fis'8. d'16~
		%a10
		d'16 g' e'8. cis'16|
		%a11
		r16 fis' d'8. g'16~
		%a12
		g'16 fis'8 e' b'16~
		%a13
		b'16 a' fis'8. d''16~
		%a14
		d''16 cis'' a'8. fis''16~
		%a15
		\sd fis''16 e''8 cis'' a'16~
		%a16
		\su a'8 d'' b''16 s|
		%a17
		\lh \sd r16 e''8 cis'' a'16~
		%a18
		\su a'8 d'' b''16 s|
		%a19
		s8 a''^"M" s4
		%a20
		a''8 s4
		%a21
		fis''8 s4
		%a22
		b''8^"M" a''|
		%a23
		gis''8 \rh[b' c'']|
		%a24
		gis'8 f''32 d'' ais' b' e'' c'' gis' a'|
		%a25
		d''32 b' fis' gis' c'' a' dis' e' b' gis' dis' e'|
		%a26
		a'8~a'32 gis' a' b' c'' gis' a' b'|
		%a27
		r8 a'4~
		%a28
		a'8 a'4~
		%a29
		a'8 cis''4^\prall~
		%a30
		cis''4.^\prall~
		%a31
		cis''4._\prall~
		%a32
		cis''4._\prall
		%a33
		\times 12/15{a''16 \sm gis''32 fis'' e'' d'' cis'' b' a' gis' fis' e' d' cis' b \n}
		%a34
		\lh a32 \rh[e' cis' a]\lh cis' \rh[a' e' cis']\lh e' \rh[cis'' a' e']|
		%a35
		\lh a'32 \rh[e'' cis'' a']\lh cis'' \rh[a'' e'' cis'']\lh a' \rh[e'' cis'' a']|
		%a36
		\lh e'32 \rh[cis'' a' e']\lh cis' \rh[a' e' cis']\lh a \rh[e' cis' a]|
		%a37
		\lh a,16 d32 e \rh a4 \bar "||";
	}
	 \context Thread=tb{ \n
		 \rh s8 fis' e'|
		 s4.
		 %a3
		 s8 fis' e'|
		 %a4
		 fis'8 s \lh a'|
		 %a5
		 s8 \rh d'' s|
		 \skip \longa*35/64;
		 %a11
		 e'16~
		 %a12
		 e'16 s s4|
		 s4.*10
		 %a23
		 s8 gis' a'|
		 %a24
		 b'8 s4|
		 s4.*6
		 %a31
		 s8 e''4^\prall~e''4.^\prall
	 }
	  \context Thread=tc{ \n
	  \rh s8 d' cis'|
	  s4.
	  %a3
	  s8 d' cis'
	  %a4
	  d'8 s \lh fis'|
	  %a5
	  s8 \rh a'
	  }>
	\autochange Staff
	\context Voice=vb< \zs
	\sd
	\context Thread=td{ \n
		\lh \sm
		%
		% WARNING: interface still subject to change!
		%
		\property Voice. beamHeight = 5
		[\times 2/3{d32 fis a}\n d'16]a8 a|
		\property Voice.beamHeight = ##f
		%b2
		d4 s8|
		%b3
		s8 a a|
		%b4
		r32[d fis a][d' fis' a' d'']r \rh[d fis a]
		%b5
		[d'32 fis' a' d''] \forceStaffDown r32 \lh[d fis a][d' fis' a' d'']
		%b6
		s a''32 s fis'' s d'' s a' s d' s a|
		%b7
		\su r8. fis16 b8|
		%b8
		r8. g16 a8~
		%b9
		a16 r8 fis16 b8|
		%b10
		r8. g16 b8|
		s4.
		%b12
		r8 a b|
		%b13
		\sd r8 d'4|
		%b14
		e'8 fis'4|
		%b15
		\su s8 a''^"M" s|
		%b16
		\sd b8 d' gis'|
		%b17
		\rh \su s8 a'' s|
		%b18
		\sd b8 d' gis'|
		%b19
		\times 2/3{a'16 c'' b' a' c'' d'' e'' c'' b'}|
		%b20
		\times 2/3{a'16 c'' b' a' c'' d'' e'' c'' b'}|
		%b21
		\times 2/3{a'16 c'' b' a' b' c''}dis''32 c'' b' a'|
		%b22
		b'32 gis' dis' e' d''8 cis''|
		%b23
		b'32 gis' dis' e' s4|
		%b24
		s8 \lh d' cis'|
		%b25
		b8 a gis|
		%b26
		a32 d' c' b c' \rs f' \zs e' d' c' e' fis' gis'|
		%b27
		[\su a8 \sd a' \su b]|
		%b28
		[a8 \sd a' \su b]|
		%b29
		[a8 \sd a' \su b]|
		%b30
		[a8 \sd a' \su b]|
		%b31
		[a8 \sd a' \su b]|
		%b32
		[a8 \sd a' \su b]|
		}
	 \context Thread=te{ \n
		 \lh s4.*24
		 %b25
		 b,8 a, gis,|
		 s4.
		 %b27
		 s4 d'8|
		 %b28
		 cis'8 s d'|
		 %b29
		 cis'8 s d'|
		 %b30
		 cis'8 s d'|
		 %b31
		 cis'8 s d'|
		 %b32
		 cis'8
	 }>
	\autochange Staff
	\context Voice=vc< \zs
	\sd
	\context Thread=tf{ \n
		\lh s4.*6
		%c7
		d4.|
		%c8
		e4.|
		%c9
		d4.|
		%c10
		e4.|
		%c11
		d8 fis cis|
		%c12
		d4 g8|
		%c13
		fis4 b8|
		%c14
		a4 d'8|
		%c15
		a8 s a_"M"|
		%c16
		s4 s16 \rh e|
		%c17
		a8 s a|
		%c18
		s4 s16 \lh e|
		%c19
		a8_"M" s a|
		%c20
		g8 s g|
		%c21
		dis8 s dis|
		%c22
		e8 s4|
		%c23
		s8 e dis|
		%c24
		e8 d cis
		}
	 \context Thread=tg{ \n
		 \lh s4.*10
	 %c11
	 s4 a8
	 }>
	 >
	\context Staff=down<
	\clef bass;
	\key d \major;
	s4.*37
	>>
	\paper { 
		%sonata-specific settings
		\translator{\VoiceContext beamAutoBegin=0;beamAutoEnd_8="3/8";
		beamAutoEnd_16="3/8";beamAutoEnd_24="1/8";beamAutoEnd_32="1/8";}
	}
}

