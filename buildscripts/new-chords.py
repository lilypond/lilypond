
# to nwe chord syntax.  
import re
import string
import sys

def sub_chord (m):
	str = m.group(1)

	if re.search (r'\\\\', str):
		return '<%s>' % str

	if re.match (r'^\s*\)?\s*\\[a-zA-Z]+', str):
		return '<%s>' % str

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


	dyns = ['']
	def sub_dyn_end (m):
		dyns.append (' -\!')
		return m.group(2)

	str = re.sub (r'(\\!)\s*([a-z]+)', sub_dyn_end, str)

	slur_strs = ['']
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
	
	pslur_strs = ['']
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

	str = re.sub ('<([^?])', r'%s\1' % simstart, str)
	str = re.sub ('>([^?])', r'%s\1' % simend,  str)
	str= re.sub ('@STARTCRESC@', r'\\<', str)
	str= re.sub ('@STARTDECRESC@', r'\\>' ,str)
	str= re.sub ('@STARTCHORD@', chordstart, str)
	str= re.sub ('@ENDCHORD@', chordend, str)
	str= re.sub (r'@ACCENT@', '>', str)
	return str


print sub_chords (open (sys.argv[1]).read())  + marker_str 
