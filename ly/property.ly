% property.ly
% list of properties that lily recognises
% and some shorthands (ugh)

%{

PROPERTIES

name			value	effect			shorthand

[Voice]
ydirection		-1	force stem down		\stemdown
ydirection		0  	stem direction free	\stemboth
ydirection		1	force stem up		\stemup

pletvisibility		0	show nothing
pletvisibility		1 	show number
pletvisibility		2 	show (number and bracket)-if-no-beam
pletvisibility		3 	show number, and bracket-if-no-beam
pletvisibility		4 	show number, and bracket

slurdash		0	normal slurs
slurdash	        1	dotted slurs
slurdash	        >1	dashed slurs

slurydirection		-1	force stem down		\slurdown
slurydirection		0  	stem direction free	\slurboth
slurydirection		1	force stem up		\slurup

slurydirection		-1	force stem down		\slurdown
slurydirection		0  	stem direction free	\slurboth
slurydirection		1	force stem up		\slurup

textalignment		-1	left alignment of text
textalignment		0	center alignment of text
textalignment		1	right alignment of text

beamAuto                0/1     auto-beam on/off
beamAutoEnd		"num/den"    end auto-beam
beamAutoEnd_8		"num/den"    end auto-beam of 8ths
beamAutoEnd_16		"num/den"    end auto-beam of 16ths

[Score?]
beamslopedamping	0	no damping		\beamslopeproportional	
beamslopedamping	1	damping1)		\beamslopedamped
beamslopedamping	100000	zero slope		\beamslopezero

[Score?]
beamquantisation	0	no quantisations	\beamposfree
beamquantisation	1	quantise pos and slope	\beamposnormal
beamquantisation	2	quantise avoide wedge2)	\beampostraditional

[Staff]
keyoctaviation	0	Key signature only for specified octave	\specialkey
keyoctaviation	1	Key signature for all octaves	\normalkey

[Staff]
barAlways		0	none
barAlways		1	generate bar at every moment

[Staff]
barAuto			0	none
barAuto			1	auto-generate bar every measure
barAtLineStart		0/1     generate bar at beginning of line

[Staff]
timeSignatureStyle	C	Use C and stroked C for 4/4,2/2
timeSignatureStyle	old	Use old style mensuration marks
timeSignatureStyle	1	Use single number
timeSignatureStyle	""	Use normal two-digit time signature
timeSignatureStyle	Cn/m	Set symbol explicitly, n/m=2/2 or 4/4	
timeSignatureStyle	oldn/m	Set symbol explicitly,
				n/m=2/2,3/2,3/4,4/4,6/4	or 9/4.
[Staff]
voltaVisibility         0/1     on/off
voltaSpannerDuration    Rat.    Coda kludge: set length of volta-spanner,
                                typically set to one measure: "1"

[Staff]
clefStyle       "fullSizeChanges" Clef changes typeset in full size
clefStyle       "transparent"     No clef typeset

[Staff?]
instrument		ascii	midi instrument table lookup

[Score]
chordInversion		0/1	Find and display chord with inversion?


1) after beam slope damping table suggested in [Wanske]
2) [Wanske] as well as [Ross] suggests that beams sloped upward must not 
   start sitting on a staffline, and beams sloped downward must not hang 
   from a staffline (similar for beam-ends).  This would create a wedge
   that is traditionally being avoided because it could easily be filled-up 
   with ink.
   However, avoiding these wedges restricts the freedom of beams quite a lot 
   while they don't seem to be a problem in modern printing.
   In no piece of sheetmusic engraved after 1953 (Baerenreiter) i've seen 
   these wedges being avoided.

%}

%hmm, (these) abbrevs suck, imo
% i guess they're meant as some form of doco
% that's what i use them for...
stemup =        \property Voice.ydirection = \up 
stemboth= 	\property Voice.ydirection = \center
stemdown = 	\property Voice.ydirection = \down

slurup = \notes {
	s1*0
	\property Voice.slurydirection = \up 
	}
slurboth= \notes {
	s1*0
	\property Voice.slurydirection = \center
}
slurdown = \notes { 	
	s1*0
	\property Voice.slurydirection = \down
}

shifton = 	\property Voice.hshift = 1
shiftoff = 	\property Voice.hshift = 0

onevoice = { 	
	\stemboth \shiftoff	
}

voiceone = 
	\context Voice = one  {
	\stemup
}

voicetwo = 
	\context Voice = two {
	\stemdown
}

voicethree = 
	\context Voice = three {
	\stemup

}

voicefour = 
	\context Voice = four {
	\stemdown
	\shifton
}

onestaff = 
	\translator Staff=one


staffone = { 	
	\translator Staff=one
	\property Staff.ydirection = \center
	\property Staff.hshift = 0
}

stafftwo = { 	
	\translator Staff=two
	\property Staff.ydirection = \center
	\property Staff.hshift = 0
}

staffthree = { 	
	\translator Staff=three
	\property Staff.ydirection = \center
	\property Staff.hshift = 0
}

stafffour = { 	
	\translator Staff=four
	\property Staff.ydirection = \center
	\property Staff.hshift = 0
}

% ugh, cluttering global namespace...

% ugh2. 
none=0
free=0
normal=1
traditional=2
infinity=10000

beamslopeproportional = 
	\property Score.beamslopedamping = \none

beamslopedamped = 
	\property Score.beamslopedamping = \normal


beamslopezero = 
	\property Score.beamslopedamping = \infinity


% this sucks, you'd want to pass an array, at least
% (or embedded code: you still can't dictate the slope / stemlength)
beamposfree = 
	\property Score.beamquantisation = \none


beamposnormal = 
	\property Score.beamquantisation = \normal


beampostraditional = 
	\property Score.beamquantisation = \traditional


slurnormal = 
	\property Voice.slurdash = ""


slurdotted = 
	\property Voice.slurdash = 1


%{
 for grace note hack, see input/test/grace.ly
%}
pletoff = {
	\property Voice.pletvisibility = 0
}
pleton = {
	\property Voice.pletvisibility = 3
}
tiny  = {
	\property Voice.fontsize= "-2"
	\pletoff %urg
}

small  = {
	\property Voice.fontsize= "-1"
}

normalsize = {
	\property Voice.fontsize= "0"
	\pleton %urg
}

%{
  [urg: try at] temporary grace note hack
  the total visible duration of the grace notes must be half
  the duration of the 'at' note: e.g.:

  \grace b8 \graceat c4 \ecarg
  \grace c16 b16 \graceat c4 \ecarg

grace = {
	\tiny
% it would be so cool not to have to specify these factors each time...
% :-(
	\property Voice.pletvisibility = 0
	\[1/16
}

graceat = \melodic {
	\normalsize
	\property Voice.pletvisibility = 0
	\] \[31/32
}

ecarg =  \melodic {
	\property Voice.pletvisibility = 0
	\]
	\property Voice.pletvisibility = 3
}
%}

normalkey = {
	\property Staff.keyoctaviation = 1
}

specialkey = {
	\property Staff.keyoctaviation = 0
}

% End the incipit and print a ``normal line start''.
endincipit = \notes{
    \partial 16; s16  % Hack to handle e.g. \bar ".|"; \endincipit
    \property Staff.clefStyle = "fullSizeChanges" 
    \nobreak \bar "";
}
