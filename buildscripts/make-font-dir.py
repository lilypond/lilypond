#!@PYTHON



## make a fonts.scale file.


import re
import sys
import string
import os


### mftrace/afm.py

# Read some global vars 
class Afm_reader:
	def __init__ (self, filename):
		self.filename = filename
		self.lines = open (self.filename).readlines ()

	def get_afm (self):
		afm = Afm_font_metric (self.filename)
		for i in self.lines[:20]:
			m = re.match ('([^ \t\n]*)[ \t]*(.*[^ \t\n])', i)
			if m and m.group (1):
				key = m.group (1)
				value = m.group (2)
				if key != 'Comment':
					afm.__dict__[key] = value
		return afm

class Afm_font_metric:
	def __init__ (self, filename):
		m = re.match ('.*/(.+)', filename)
		self.filename = m.group (1)
		m = re.match ('([-_A-Za-z]*)([0-9]*)', self.filename)
		self.name = m.group (1) + m.group (2)
		self.basename = m.group (1)
		self.designsize = m.group (2)
	
def read_afm_file (filename):
	reader = Afm_reader (filename)
	return reader.get_afm ()

#if __name__ == '__main__':
#	i = read_afm_file  (sys.argv[1])
#	print i, i.FullName, i.FontName

### mftrace

class Font_info:
	cm = {
		'bx': ('bold', 'roman'),
		'bxti' : ('bold', 'italic'),
		'csc' : ('smallcaps', 'roman'),
		'r' : ('regular', 'roman'),
		'tt' : ('regular', 'typewriter'),
		'ti' : ('regular', 'italic'),
		}
	      
	def set_defaults (self, name):
		self.FontName = name
		self.FullName = name
		self.EncodingScheme = 'AdobeStandard'
		
		self.foundry = 'GNU'
		self.family = 'LilyPond'
		self.weight = 'Feta'
		self.slant = 'r'
		self.setwidth = 'normal'
		self.style = ''
		self.pixelsize = '' # '0'
		self.pointsize = '0'
		self.xresolution = '0'
		self.yresolution = '0'
		self.spacing = 'p'
		self.averagewidth = '0'
		self.registry = 'GNU'
		self.encoding = 'FontSpecific'

		split = string.split (name, '-')
		if len (split) >= 4:
			# Assume
			#   Adobe FontName = X11 foundry-family-weight-style
			self.foundry, self.family = split[:2]
			self.weight = string.join (split[2:-1], ' ')
			self.style = split[-1:][0]
			self.FamilyName = '%s %s' % (self.family, self.weight)
			self.designsize = self.style
		elif name[:2] == 'cm':
			self.foundry = 'TeX' # Knuth?
			self.FamilyName = 'Computer Modern'
			self.family = self.FamilyName
			m = re.match ('^cm([a-z]*)([0-9]*)', name)
			self.weight = string.join (self.cm[m.group (1)], ' ')
			self.designsize = m.group (2)
			self.style = self.designsize
		else:
			self.FamilyName = name

	def __init__ (self, x):
		if type (x) == type ("hallo"):
			m = re.match ('([-_A-Za-z]*)([0-9]*)', x)
			self.name = x
			self.basename = m.group (1)
			self.designsize = m.group (2)
			self.set_defaults (x)
		elif type (x) == type ({}):
			self.set_defaults (x['FontName'])
			for k in x.keys ():
				self.__dict__[k] = x[k]

	def __getitem__ (self, key):
		return self.__dict__[key]

	def get_X11 (self):
		return (self.foundry, self.family, self.weight,
			self.slant, self.setwidth, self.style,
			self.pixelsize, self.pointsize,
			self.xresolution, self.yresolution,
			self.spacing, self.averagewidth,
			self.registry, self.encoding)

fontinfo = {}

# wat een intervaas...
ls =  sys.stdin.readline ()
ls = string.split (ls)

sketch_p = 0
if len (ls) and ls[0] == 'sketch':
	ls = ls[1:]
	sketch_p = 1

if not sketch_p:
	print len(ls)
	
for filename in ls:
	basename = re.sub ('\.pf[ab]', '',filename)	
	fontname = re.sub ('-', ' ',basename)

	m = re.search ("([0-9]+)$", fontname)
	designsize = 'normal'

	
	if m:
		designsize =  m.group (1)
		fontbase = re.sub ("([0-9]+)$", "", fontname)
		

	# FIXME: Font naming  -- what a mess
	# Check sane naming with xfontsel and gtkfontsel

	# Adobe's font naming scheme and X11's seem to be conflicting.
	# Adobe's FontFamily seems to be X11's family + weight
	# Also, text selection applets like gtkfontsel, gfontview and
	# GNOME-applications specific ones, display X11's `family'
	# parameter as `Font', and X11's `Weight' parameter as `Style'.

	# Using X11 description/convention -- good for xfontsel:
	# foundry: GNU
	# family: LilyPond <basename>
	# weight: <designsize>
	# slant: r(oman) =upright
	# setwidth: normal
	# style:
	# pixelsize: 0
	# pointsize: 0 (20 crashes xfs, moved to style)
	# xresolution: 0
	# yresolution: 0
	# spacing: p(roportional)
	# averagewidth: 0
	# registry: GNU
	# encoding: fonstpecific

	# gives:
	# feta20.pfa -GNU-LilyPond feta-20-r-normal--0-0-0-0-p-0-gnu-fontspecific

	# However, GNOME (gtkfontsel, gnome apps) seems to want:

	# foundry: GNU
	# family: LilyPond
	# weight:  <basename>
	# slant: r(oman) =upright
	# setwidth: normal
	# style: <designsize>
	# pixelsize: 0
	# pointsize: 0 (20 crashes xfs, moved to style)
	# xresolution: 0
	# yresolution: 0
	# spacing: p(roportional)
	# averagewidth: 0
	# registry: GNU
	# encoding: fonstpecific

	# which gives:
	# feta20.pfa -GNU-LilyPond-feta-r-normal--20-0-0-0-p-0-gnu-fontspecific
	# foundry: GNU
	
	## ouch, pointsize 20 crashes xfs
	## XXXfeta20.pfa -GNU-LilyPond Feta-regular-r-normal--0-20-0-0-p-0-gnu-fontspecific

	## feta20.pfa -GNU-LilyPond feta-regular-r-normal-20-0-0-0-0-p-0-gnu-fontspecific

	afmfile = ''
	if not afmfile:
		#afmfile = find_file (basename + '.afm')
		afmfile = basename + '.afm'
		
	if afmfile:
		afmfile = os.path.abspath (afmfile)
	if os.path.exists (afmfile):	
		afm = read_afm_file (afmfile)
		fontinfo = Font_info (afm.__dict__)
	else:
		fontinfo = Font_info (basename)
		
	family_name = string.join (string.split (fontinfo['FamilyName'],
							'-'), ' ')
	if not sketch_p:
		print filename + ' -' + string.join (fontinfo.get_X11 (), '-')
		
	else:
		# Sketch's lilypond.sfd map:
		s = string.join ([fontinfo.FontName,
				  fontinfo.family,
				  '%s %s' % (fontinfo.weight, fontinfo.style),
				  string.join (fontinfo.get_X11 ()[:5], '-'),
				  string.join (fontinfo.get_X11 ()[:-2], '-'),
				  fontinfo.name],
				 ',')
		print s

		s = string.join ([fontinfo.FamilyName,
				  fontinfo.family,
				  '%s %s' % (fontinfo.weight, fontinfo.style),
				  string.join (fontinfo.get_X11 ()[:5], '-'),
				  string.join (fontinfo.get_X11 ()[:-2], '-'),
				  fontinfo.name],
				 ',')
		print s
