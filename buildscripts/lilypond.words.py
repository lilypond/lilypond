#!@PYTHON@

# Created 01 September 2003 by Heikki Junes.
# Generates lilypond.words.el for (X)Emacs and lilypond.words.vim for Vim.

import string
import re
import sys

kw = []
rw = []
notes = []

# keywords not otherwise found
for line in ['include','maininput','version']:
    kw = kw + [line]

# the main keywords
F = open('lily/my-lily-lexer.cc', 'r')
for line in F.readlines():
    m = re.search(r"(\s*{\")(.*)(\",\s*.*},\s*\n)",line)
    if m:
	kw = kw + [m.group(2)]
F.close()

# keywords in markup
F = open('scm/new-markup.scm', 'r')
for line in F.readlines():
    m = re.search(r"^(\s*\(cons\s*)([a-z-]*)(-markup)",line)
    if m:
	kw = kw + [m.group(2)]
F.close()

# identifiers and keywords
for name in [
'ly/a4-init.ly',
'ly/chord-modifiers-init.ly',
'ly/dynamic-scripts-init.ly',
'ly/engraver-init.ly',
'ly/grace-init.ly',
'ly/gregorian-init.ly',
'ly/performer-init.ly',
'ly/property-init.ly',
'ly/scale-definitions-init.ly',
'ly/script-init.ly',
'ly/spanners-init.ly',
]:
    F = open(name, 'r')
    for line in F.readlines():
        m = re.search(r"^([a-zA-Z]+)(\s*=)",line)
        if m:
	    kw = kw + [m.group(1)]
    F.close()

# more identifiers
for name in [
'ly/declarations-init.ly',
'ly/paper11-init.ly',
'ly/paper13-init.ly',
'ly/paper16-init.ly',
'ly/paper19-init.ly',
'ly/paper20-init.ly',
'ly/paper23-init.ly',
'ly/paper26-init.ly',
'ly/paper-as5-init.ly',
'ly/paper-as9-init.ly',
'ly/paper-init.ly',
'ly/params-init.ly',
'ly/params-as-init.ly',
]:
    F = open(name, 'r')
    for line in F.readlines():
        m = re.search(r"^(\s*)([a-zA-Z]+)(\s*=)",line)
        if m:
	    kw = kw + [m.group(2)]
    F.close()

# note names
for name in [
'ly/catalan.ly',
'ly/deutsch.ly',
'ly/drumpitch-init.ly',
'ly/english.ly',
'ly/espanol.ly',
'ly/italiano.ly',
'ly/nederlands.ly',
'ly/norsk.ly',
'ly/suomi.ly',
'ly/svenska.ly',
]:
    F = open(name, 'r')
    for line in F.readlines():
	m = re.search(r"^(\s*\()([a-z]+)([^l]+ly:make-pitch)",line)
	if m:
	    notes = notes + ['' + m.group(2)]
    F.close()

# (short) drum names
for name in [
'share/lilypond/scm/drums.scm'
]:
    F = open(name, 'r')
    for line in F.readlines():
	m = re.search(r"^(\s*\([a-z]+\s*)([a-z]+)(\s*,\(ly:make-pitch)",line)
	if m:
	    notes = notes + ['' + m.group(2)]
    F.close()
    
# reserved words
for name in [
'ly/engraver-init.ly',
'ly/performer-init.ly',
]:
    F = open(name, 'r')
    for line in F.readlines():
      	for pattern in [
	r"^(\s*.consists\s+\")([a-zA-Z_]+)(\")",
	r"([\\]name\s+[\"]?)([a-zA-Z_]+)([\"]?)",
	r"(\s+)([a-zA-Z_]+)(\s*[\\]((set)|(override)))",
	]:
	    m = re.search(pattern,line)
	    if m:
	        rw = rw + ['' + m.group(2)]
    F.close()

# the output file
if sys.argv[1:] == []:
  out_el = open('lilypond.words.el', 'w')
  out_vim = open('lilypond.words.vim', 'w')
else:
  out_el = open(sys.argv[1]+'/lilypond.words.el', 'w')
  out_vim = open(sys.argv[1]+'/lilypond.words.vim', 'w')
   
# alphabetically ordered words
kw.sort()
kw.reverse()
prevline = ''
out_vim.write('syn match lilyKeyword \"[-_^]\\?\\\\\\(');
for line in kw:
    if line != prevline:
        out_el.write('\\\\' + line + '\n')
	out_vim.write(line + '\\|')
    prevline = line
out_vim.write('n\\)\\(\\A\\|\\n\\)\"me=e-1\n')

rw.sort()
rw.reverse()
prevline = ''
out_vim.write('syn match lilyReservedWord \"\\(\\A\\|\\n\\)\\(');
for line in rw:
    if line != prevline:
        out_el.write(line + '\n')
	out_vim.write(line + '\\|')
    prevline = line
out_vim.write('Score\\)\\(\\A\\|\\n\\)\"ms=s+1,me=e-1\n')

notes.sort()
notes.reverse()
prevline = ''
out_vim.write('syn match lilyNote \"\\<\\(\\(\\(');
for line in notes:
    if line != prevline:
        out_el.write(line + '\n')
	out_vim.write(line + '\\|')
    prevline = line
out_vim.write('a\\)\\([,\']\\)\\{,4}\\([?!]\\)\\?\\)\\|s\\|r\\)\\(\\(128\\|64\\|32\\|16\\|8\\|4\\|2\\|1\\|\\\\breve\\|\\\\longa\\|\\\\maxima\\)[.]\\{,8}\\)\\?\\(\\A\\|\\n\\)\"me=e-1\n')

# the menu in lilypond-mode.el
for line in [
'/( - _ /) -',
'/[ - _ /] -',
'< - _ > -',
'<< - _ >> -',
'///( - _ ///) -',
'///[ - _ ///] -',
'///< - _ ///! -',
'///> - _ ///! -',
'//center - / << _ >> -',
'//column - / << _ >> -',
'//context/ Staff/ = - % { _ } -',
'//context/ Voice/ = - % { _ } -',
'//markup - { _ } -',
'//notes - { _ } -',
'//relative - % { _ } -',
'//score - { //n /? //simultaneous { //n _ //n } /! //n //paper {  } //n /? //midi {  } //n /! } //n -',
'//simultaneous - { _ } -',
'//sustainDown - _ //sustainUp -',
'//times - % { _ } -',
'//transpose - % { _ } -',
]:
    # urg. escape char '/' is replaced with '\\' which python writes as a '\'.
    out_el.write(string.join(string.split(line,'/'),'\\') + '\n')
 
out_el.close()
out_vim.close()
