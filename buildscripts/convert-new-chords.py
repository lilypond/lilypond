
# to nwe chord syntax.  
import re
import string
import sys
import getopt
import os


def sub_chord (m):
	str = m.group(1)

	origstr =  '<%s>' % str
	if re.search (r'\\\\', str):
		return origstr

	if re.search (r'\\property', str):
		return origstr

	if re.match (r'^\s*\)?\s*\\[a-zA-Z]+', str):
		return origstr

	durs = []
	def sub_durs (m):
		durs.append(m.group(2))
		return m.group (1)

	str = re.sub ("([a-z]+[,'!? ]*)([0-9.]+)", sub_durs, str)
	dur_str = ''

	for d in durs:
		if dur_str == '':
			dur_str = d
		if dur_str <> d:
			return '<%s>' % m.group (1)

	pslur_strs = ['']
	dyns = ['']
	slur_strs = ['']

	last_str = ''
	while last_str <> str:
	  last_str = str
	  def sub_dyn_end (m):
		  dyns.append (' -\!')
		  return ' ' + m.group(2)

	  str = re.sub (r'(\\!)\s*([a-z]+)', sub_dyn_end, str)
	  def sub_slurs(m):
		  if '-)' not in slur_strs:
			  slur_strs.append ( '-)')
		  return m.group(1)
	  def sub_p_slurs(m):
		  if '-\)' not in slur_strs:
			  slur_strs.append ( '-\)')
		  return m.group(1)
	  str = re.sub (r"\)[ ]*([a-z]+)", sub_slurs, str)
	  str = re.sub (r"\\\)[ ]*([a-z]+)", sub_p_slurs, str)
	  def sub_begin_slurs(m):
		  if '-(' not in slur_strs:
			  slur_strs.append ( '-(')
		  return m.group(1)
	  str = re.sub (r"([a-z]+[,'!?0-9 ]*)\(", sub_begin_slurs, str)
	  def sub_begin_p_slurs(m):
		  if '-\(' not in slur_strs:
			  slur_strs.append ( '-\(')
		  return m.group(1)

	  str = re.sub (r"([a-z]+[,'!?0-9 ]*)\\\(", sub_begin_p_slurs, str)

	  def sub_dyns (m):
		  s = m.group(0)
		  if s == '@STARTCRESC@':
			  slur_strs.append ("-\\<")
		  elif s == '@STARTDECRESC@':
			  slur_strs.append ("-\\>")
		  elif s == r'-?\\!':
			  slur_strs.append ('-\\!')
		  return ''

	  str = re.sub (r'@STARTCRESC@', sub_dyns, str)
	  str = re.sub (r'-?\\!', sub_dyns, str)

	  def sub_articulations (m):
		  a = m.group(1)
		  if a not in slur_strs:
			  slur_strs.append (a)
		  return ''

	  str = re.sub (r"([_^-]\@ACCENT\@)", sub_articulations, str)
	  str = re.sub (r"([_^-]\\[a-z]+)", sub_articulations, str)
	  str = re.sub (r"([_^-][>_.+|^-])", sub_articulations, str)

	  def sub_pslurs(m):
		  slur_strs.append ( ' -\\)')
		  return m.group(1)
	  str = re.sub (r"\\\)[ ]*([a-z]+)", sub_pslurs, str)

	suffix = string.join (slur_strs, '') + string.join (pslur_strs, '') \
		 + string.join (dyns, '')

	return '@STARTCHORD@%s@ENDCHORD@%s%s' % (str , dur_str, suffix)




simend = '}'
simstart = "\n\\simultaneous {"
chordstart = '<'
chordend = '>'

old_syntax = 1

if old_syntax:
	simend = '>'
	simstart = "<" 
	chordstart = '<<'
	chordend = '>>'


marker_str = '%% new-chords-done %%'

def sub_chords (str):
	if re.search (marker_str,str):
		return str
	str= re.sub (r'\\<', '@STARTCRESC@', str)
	str= re.sub (r'\\>', '@STARTDECRESC@', str)
	str= re.sub (r'([_^-])>', r'\1@ACCENT@', str)
	str = re.sub ('<([^<>{}]+)>', sub_chord, str)
	str = re.sub (r'\\! *@STARTCHORD@([^@]+)@ENDCHORD@',
		      r'@STARTCHORD@\1@ENDCHORD@-\!',
		      str)
	str = re.sub ('<([^?])', r'%s\1' % simstart, str)
	str = re.sub ('>([^?])', r'%s\1' % simend,  str)
	str = re.sub ('@STARTCRESC@', r'\\<', str)
	str = re.sub ('@STARTDECRESC@', r'\\>' ,str)
	str = re.sub (r'\\context *Voice *@STARTCHORD@', '@STARTCHORD@', str)
	str = re.sub ('@STARTCHORD@', chordstart, str)
	str = re.sub ('@ENDCHORD@', chordend, str)
	str = re.sub (r'@ACCENT@', '>', str)
	return str

def articulation_substitute (str):
	str = re.sub (r"""([^-])\[ *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
		      r" \1 \2-[", str)
	str = re.sub (r"""([^-])\) *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
		      r"\1 \2-)", str)
	str = re.sub (r"""([^-])\\! *([a-z]+[,']*[!?]?[0-9:]*\.*)""",
		      r"\1 \2-\\!", str)
	return str

def help ():
	print r"""
new-chords.py -- update .ly files to new syntax.

Usage:
  new-chords.py [OPTIONS] FILE(S)

Options

  -e, --edit     in-place edit
  -h, --help     this help

Description

  This script converts old chord notation to new chord notation, i.e.

     \< <a )b>

  becomes

     <<a b>> -\< -)

  It will also convert slur-end, beam-start and cresc-end to postfix
  notation, i.e.

    [ \! )a

  becomes

    a-\!-)-[ 

  By default, the script will print the result on stdout. Use with -e
  if you are confident that it does the right thing.

Warning

  This conversion does not convert all files correctly. It is
  recommended to verify the output of the new file manually.
  In particular, files with extensive Scheme code (markups, like

     #'(italic "foo")

  and Scheme function definitions may be garbled by the textual
  substitution.

"""


(opts, files)= getopt.getopt( sys.argv[1:], 'eh',['help','edit'])
edit = 0
for (o,a) in opts:
	if o == '-e' or o == '--edit':
		edit = 1
	if o == '-h' or o == '--help':
		help ()
		sys.exit (0)

if not files:
	print 'Error: no input files.\n use -h for help.'
	sys.exit(2)
	

for a in files:
	str = open (a).read()
	if re.search (marker_str, str):
		continue

	sys.stderr.write ("processing %s\n" %a)
	
	str = sub_chords (str)  + marker_str + '\n'
	str = articulation_substitute (str)

	if edit:
		open (a + '.NEW', 'w').write (str)
		os.rename (a, a + '~')
		os.rename (a + '.NEW', a)
	else:
		print str


##
## regexes for postfix slur & beam:
##
#PYTHON
##
#EMACS
## \([^-]\)\[ *\([a-z]+[!?]?[,']*[0-9:]*\.*\)
#### ->
## \1 \2-[
##
## \([^-]\)) *\([a-z]+[!?]?[,']*[0-9:]*\.*\)
#### ->
## \1 \2-)

