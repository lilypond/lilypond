#!@PYTHON@

# Created 01 September 2003 by Heikki Junes.
# Generates lilypond-words.el for (X)Emacs and lilypond-words[.vim] for Vim.

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
F = open('lily/lily-lexer.cc', 'r')
for line in F.readlines():
  m = re.search(r"(\s*{\")(.*)(\",\s*.*},\s*\n)",line)
  if m:
    kw = kw + [m.group(2)]
F.close()

# keywords in markup
F = open('scm/markup.scm', 'r')
for line in F.readlines():
  m = re.search(r"^(\s*\(cons\s*)([a-z-]*)(-markup)",line)
  if m:
    kw = kw + [m.group(2)]
F.close()

# identifiers and keywords
for name in [
'ly/chord-modifiers-init.ly',
'ly/dynamic-scripts-init.ly',
'ly/engraver-init.ly',
'ly/grace-init.ly',
'ly/gregorian-init.ly',
'ly/music-functions-init.ly',
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
'ly/declarations-init.ly',
'ly/params-init.ly',
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
'ly/portugues.ly',
'ly/suomi.ly',
'ly/svenska.ly',
'ly/vlaams.ly',
]:
  F = open(name, 'r')
  for line in F.readlines():
    m = re.search(r"^(\s*\()([a-z]+)([^l]+ly:make-pitch)",line)
    if m:
      notes = notes + ['' + m.group(2)]
  F.close()



# reserved words
for name in ['ly/engraver-init.ly',
             'ly/performer-init.ly']:
  f = open(name, 'r')
  for line in f.readlines():
      for pattern in [r"^(\s*.consists\s+\")([a-zA-Z_]+)(\")",
                      r"([\\]name\s+[\"]?)([a-zA-Z_]+)([\"]?)",
                      r"(\s+)([a-zA-Z_]+)(\s*[\\]((set)|(override)))"]:
          m = re.search(pattern,line)
          if m:
              rw = rw + ['' + m.group(2)]

# the output file
outdir = '.';
suffix = ['skip','skip','skip'];
outs  = ['','',''];
for s in sys.argv[1:]:
  if s == '--words':
    suffix[0] = '';
  if s == '--el':
    suffix[1] = '.el';
  if s == '--vim':
    suffix[2] = '.vim';
  m = re.search(r"(--dir=)(\S*)",s)
  if m:
    outdir = m.group(2)

if '' in suffix:
  outs[0] = open(outdir+'/lilypond-words'+suffix[0], 'w')
if '.el' in suffix:
  outs[1] = open(outdir+'/lilypond-words'+suffix[1], 'w')
if '.vim' in suffix:
  outs[2] = open(outdir+'/lilypond-words'+suffix[2], 'w')

# alphabetically ordered words
kw.sort()
kw.reverse()
prevline = ''
if '.vim' in suffix:
 outs[2].write('syn match lilyKeyword \"[-_^]\\?\\\\\\(');
for line in kw:
  if line != prevline:
    if '' in suffix:
        outs[0].write('\\\\' + line + '\n')
    if '.el' in suffix:
        outs[1].write('\\\\' + line + '\n')
    if '.vim' in suffix:
        outs[2].write(line + '\\|')
  prevline = line
if '.vim' in suffix:
  outs[2].write('n\\)\\(\\A\\|\\n\\)\"me=e-1\n')

rw.sort()
rw.reverse()
prevline = ''
if '.vim' in suffix:
  outs[2].write('syn match lilyReservedWord \"\\(\\A\\|\\n\\)\\(')
for line in rw:
  if line != prevline:
    if '' in suffix:
      outs[0].write(line + '\n')
    if '.el' in suffix:
      outs[1].write(line + '\n')
    if '.vim' in suffix:
        outs[2].write(line + '\\|')
  prevline = line
if '.vim' in suffix:
  outs[2].write('Score\\)\\(\\A\\|\\n\\)\"ms=s+1,me=e-1\n')

notes.sort()
notes.reverse()
prevline = ''
if '.vim' in suffix:
  outs[2].write('syn match lilyNote \"\\<\\(\\(\\(');
for line in notes:
  if line != prevline:
    if '' in suffix:
      outs[0].write(line + '\n')
    if '.el' in suffix:
      outs[1].write(line + '\n')
    if '.vim' in suffix:
        outs[2].write(line + '\\|')
  prevline = line
if '.vim' in suffix:
  outs[2].write('a\\)\\([,\']\\)\\{,4}\\([?!]\\)\\?\\)\\|s\\|r\\)\\(\\(128\\|64\\|32\\|16\\|8\\|4\\|2\\|1\\|\\\\breve\\|\\\\longa\\|\\\\maxima\\)[.]\\{,8}\\)\\?\\(\\A\\|\\n\\)\"me=e-1\n')

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
  if '.el' in suffix:
    outs[1].write(string.join(string.split(line,'/'),'\\') + '\n')

if '' in suffix:
  outs[0].close()
if '.el' in suffix:
  outs[1].close()
if '.vim' in suffix:
  outs[2].close()
