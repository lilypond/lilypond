#!@PYTHON@
import sys
import getopt
import re 
import os

(options, files) = \
  getopt.getopt (sys.argv[1:],
		 '',
		 ['dir='])


outdir = ''
for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--dir':
		outdir = a
	else:
		print o
		raise getopt.error


for design_size in [11,13,14,16,18,20,23,26]:
	name = 'bigcheese' 
	script = '''#!@FONTFORGE@
New();

# Separate Feta versioning?
# Naming: * expose LilyPond20/LilyPond rather than %(name)s
#         *  using 20 as Weight works for gnome-font-select widget: gfs
SetFontNames("%(name)s%(design_size)d", "LilyPond", "LilyPond %(name)s %(design_size)d", "%(design_size)d", "GNU GPL", "@TOPLEVEL_VERSION@");

MergeFonts("feta%(design_size)d.pfa");
MergeFonts("parmesan%(design_size)d.pfa");

# load nummer/din after setting PUA.
i = 0;
while (i < CharCnt())
  Select(i);
# crashes fontforge, use PUA for now -- jcn
# SetUnicodeValue(i + 0xF0000, 0);
/*
PRIVATE AREA
       In the BMP, the range 0xe000 to 0xf8ff will never be  assigned  to  any
       characters  by  the standard and is reserved for private usage. For the
       Linux community, this private area has been subdivided further into the
       range  0xe000  to 0xefff which can be used individually by any end-user
       and the Linux zone in the range 0xf000 to 0xf8ff where  extensions  are
       coordinated  among  all  Linux  users.  The  registry of the characters
       assigned to the Linux zone is currently maintained by  H.  Peter  Anvin
       <Peter.Anvin@linux.org>.
*/
  SetUnicodeValue(i + 0xE000, 0);
  ++i;
endloop


MergeFonts("feta-alphabet%(design_size)d.pfa");
MergeKern("feta-alphabet%(design_size)d.tfm");

LoadTableFromFile("LILC", "feta%(design_size)d.otf-table")

Generate("%(name)s%(design_size)d.otf");
Generate("%(name)s%(design_size)d.cff");''' % vars()

	path = os.path.join (outdir, name + '%d' % design_size +  '.pe')
	open (path, 'w').write (script)

	path = os.path.join (outdir, name + '%d' % design_size +  '.dep')

	deps = r'''%(name)s%(design_size)d.otf: $(outdir)/feta%(design_size)d.pfa \
  $(outdir)/parmesan%(design_size)d.pfa  \
  $(outdir)/feta-alphabet%(design_size)d.pfa 
''' % vars()
	open (path, 'w').write (deps)
