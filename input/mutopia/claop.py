import sys


copyright = """copyright 1992. Peter Wallin. Verbatim redistribution
permitted """



"""

This file produces the Score for CLA(O)P II  -- a piece by
Peter Wallin (pwallin@post8.tele.dk).

The score produced by this file is
copyright 1992. Peter
Wallin. Verbatim redistribution permitted




From: Rune Zedeler <rune@zedeler.dk>
Sender: lilypond-devel-admin@gnu.org
To: LilyPond Devel <lilypond-devel@gnu.org>
Subject: CLA(O)P II
Date: Wed, 12 Jun 2002 02:14:05 +0200

The concert went well.
I talked to the composer and he agreed to include the score in lilypond 
as long as his name was maintained and as long as he kept the copyrights 
- that is people are not allowed to make changes to the score.

I think including CLA(O)P II is a Good Thing because it stress tests 
lilypond very well. This is also why I think that including only an 
excerpt from it is stupid.
I am not sure how the inclusion should be.
The raw lilypond file is 750k; gzip'ed 40k.
Alternatively one could include the generating c-file into lilypond.
Or perhaps we should just wait till I manage to redo it in scheme... :-)

-Rune

"""



#//set to 1, 385 to typeset the whole score
start_measure =1
no_measures = 385
voices = 48

durs = ["16","8","8.","4"]
grund = [
  "x x x x xx x",
  "x x xx xx x xx x",
  "xx xx x xx xx x xx xx x ",
  
  "x x x xx xx ",
  "x xx xx x xx xx ",
  "x xx x xx xx x xx xx x x",
  
  "x x xx xx x ",
  "xx xx x xx xx x ",
  "xx x xx xx x xx xx x xx ",
  
  "x xx xx x x ",
  "x xx x xx xx x x",
  "x x xx xx x xx xx x xx x",
  
  "xx xx x x x ",
  "xx x xx xx x xx ",
  "x xx xx x xx xx x xx xx ",
  
  "x xx x x x x",
  "x x xx xx x xx x",
  "xx xx x xx xx x xx xx x ",
  
  "xx x x x xx ",
  "x xx xx x xx xx ",
  "x xx x xx xx x xx xx x x",
  
  
  "x x x x xx x",
  "xx xx x xx xx x ",
  "xx x xx xx x xx xx x xx ",
  
  "x x x xx xx ",
  "x xx x xx xx x x",
  "x x xx xx x xx xx x xx x",
  
  "x x xx xx x ",
  "xx x xx xx x xx ",
  "x xx xx x xx xx x xx xx ",
  
  "x xx xx x x ",
  "x x xx xx x xx x",
  "xx xx x xx xx x xx xx x ",
  
  "xx xx x x x ",
  "x xx xx x xx xx ",
  "x xx x xx xx x xx xx x x",
  
  "x xx x x x x",
  "xx xx x xx xx x ",
  "xx x xx xx x xx xx x xx ",
  
  "xx x x x xx ",
  "x xx x xx xx x x",
  "x x xx xx x xx xx x xx x",
  
  "x x x x xx x",
  "xx x xx xx x xx ",
  "x xx xx x xx xx x xx xx ",
  
  "x x x xx xx ",
  "x x xx xx x xx x",
  "xx xx x xx xx x xx xx x " 
]

accents = ''
accstr = '' 


def stemme(st) :
  adr = [0] * (385*16)
  pos = st*48+44

  for i in range(0,100):
    adr[384*16-i*28] = ord ('X')

  for k in range(0,48):
    if k: 
      for j in range(0,8):
        adr[pos] = 0
	pos += 1
	
    for j in range(0,4):
      for c in grund[k]:
	  if c ==' ' :
	      adr[pos] = 0
	  else:
	      adr[pos] = 3-j+ord ('a')
	  pos += 1
	  
  for i in range(0,385*16):
    ac = accents[48*4+i-((48+24-st)%48)*4]
    if ac:
      adr[i]=ac
  
  return adr


def print1(ch) :
  accstr=""
  if ch >= ord('A') and ch <=ord('D'):
    ch += ord('a')-ord('A')
    accstr="->"

  namestr = ''
  cr = chr (ch)
  
  if cr=='a':
      namestr="a"
  elif cr=='b':
      namestr="b"
  elif cr=='c':
      namestr="d'"
  elif cr=='d':
      namestr="e'"
  elif cr=='X':
      namestr="\\override Staff.NoteHead #'style = #'cross c'"
      accstr="-^ \\revert Staff.NoteHead #'style"
  else:
      print 'foo', cr, chr(ch), ch == 'd', "A%sA" % cr
      raise 'foo'

  return (namestr,accstr)


namestr = ''
accstr = ''

def print4(ptr):
    val = reduce (lambda x,y : x|y,  ptr)
#    sys.stderr.write ('%d ' % val)
    if not val:
	sys.stdout.write ("r4 ")
    else:
	dur=-1

	global namestr
	global accstr
	if ptr[0]==0:
	    sys.stdout.write ("~")
            (namestr, accstr) = print1(val)

        for c in ptr:
	    if c==0:
		dur += 1
	    else:
		if dur>=0:
		    sys.stdout.write( "%s%s%s " % (namestr,durs[dur],accstr))
		(namestr, accstr)  = print1(c)
		dur=0
      
        sys.stdout.write( "%s%s%s "% (namestr,durs[dur],accstr))
  


def print8(ptr) :
    val = reduce (lambda x,y : x|y,  ptr)
    if val:
	print4(ptr[0:4])
	print4(ptr[4:8])
    else:
	sys.stdout.write ("r2 ")


def print16(ptr):
    val = reduce (lambda x,y : x|y,  ptr)
    if val:
	print8(ptr[0:8])
	print8(ptr[8:16])
    else:
	sys.stdout.write("R1 ")  

accents = [0] *(500*16)
for i in range(0,32):
    pos=i*12*16+4*48
    accents[pos]=ord('D')
    if(i<31) :
	if(i>0):
	    accents[pos+4*12]=ord('A')
	accents[pos+4*23]=ord('C')
	accents[pos+4*35]=ord('B' )


sys.stdout.write ('%%{\n %s \n %%}' % copyright)
sys.stdout.write (r"""
\version "2.7.29"
#(set-global-staff-size 11)

\header {
    title = "CLA(O)P II"
    composer = "Peter Wallin (1992-93)"
    copyright = "Copyright  (1992-93) Peter Wallin, verbatim redistribution permitted"
}


""")





for st in range(1,voices+1):
    str=stemme(st)
    ststr=chr((st-1)/24+ord('A')) + chr ((st-1)%24+ord('A'))
    sys.stdout.write(r'''

stemme%s =  {
\clef percussion
\set Staff.instrument = "%d"
\set Staff.instr = "%d"
\set Score.currentBarNumber = #%d
'''
    % (ststr,st,st,start_measure))
    
    for i in range(start_measure-1, start_measure-1+no_measures):
      print16(str[i*16:i*16+16])
      sys.stdout.write (" \n")
    
    sys.stdout.write ("\\bar\"|.\" }\n")
    

sys.stdout.write (r"""
<<
  \override Score.BarNumber  #'padding = #2.5
  %#(override-auto-beam-setting '(end * * * *) 1 4)
  \set Score.skipBars = ##t
  \context StaffGroup <<
    \override StaffGroup.Stem #'direction = #UP
""")


for   st in range(1,voices+1):
  ststr=chr((st-1)/24+ord('A')) + chr ((st-1)%24+ord('A'))
  ststr = 'stemme' + ststr
  sys.stdout.write (r"""\context Staff="%s" \%s
""" % (ststr,ststr))

sys.stdout.write (r""">>
>>
#(set-default-paper-size "a3")
\paper {
	linewidth = 26.0\cm
	indent = 0
	textheight = 38.0\cm
	%hsize = 30.0 \cm
	%vsize = 42.0 \cm
}
\layout {
  \context {
    \Staff 
    \override StaffSymbol #'line-count  = #3
    minimumVerticalExtent = #'(-3 . 3)
  }
}
""")
