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

# Ugh
for design_size in [11,13,14,16,18,20,23,26]:
    name = 'Emmentaler' 
    filename = name.lower ()
    script = '''#!@FONTFORGE@

New();

# Separate Feta versioning?
#         *  using 20 as Weight works for gnome-font-select widget: gfs

notice = "";
notice += "This font is distributed under the GNU General Public License. ";
notice += "As a special exception, if you create a document which uses ";
notice += "this font, and embed this font or unaltered portions of this ";
notice += "font into the document, this font does not by itself cause the ";
notice += "resulting document to be covered by the GNU General Public License.";;

SetFontNames("%(name)s-%(design_size)d", "%(name)s", "%(name)s %(design_size)d", "%(design_size)d", notice, "@TOPLEVEL_VERSION@");

MergeFonts("feta%(design_size)d.pfb");
MergeFonts("parmesan%(design_size)d.pfb");

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


MergeFonts("feta-alphabet%(design_size)d.pfb");
MergeKern("feta-alphabet%(design_size)d.tfm");

LoadTableFromFile("LILF", "%(filename)s-%(design_size)d.subfonts");
LoadTableFromFile("LILC", "feta%(design_size)d.otf-table");
LoadTableFromFile("LILY", "feta%(design_size)d.otf-gtable");

Generate("%(filename)s-%(design_size)d.otf");
Generate("%(filename)s-%(design_size)d.svg");
''' % vars()

    basename = '%s-%d' % (filename, design_size)
    path = os.path.join (outdir, basename + '.pe')
    open (path, 'w').write (script)

    subfonts = ['feta%(design_size)d',
          'parmesan%(design_size)d',
          'feta-alphabet%(design_size)d']

    ns = []
    for s in subfonts:
        ns.append ('%s' % (s % vars()))
        
    subfonts_str = ' '.join (ns)
    
    open (os.path.join (outdir, '%(filename)s-%(design_size)d.subfonts' % vars()), 'w').write (subfonts_str)

    path = os.path.join (outdir, '%s-%d.dep' % (filename, design_size))

    deps = r'''%(filename)s-%(design_size)d.otf: $(outdir)/feta%(design_size)d.pfa \
 $(outdir)/parmesan%(design_size)d.pfa  \
 $(outdir)/feta-alphabet%(design_size)d.pfa feta%(design_size)d.otf-table \
 $(outdir)/feta-alphabet%(design_size)d.pfa feta%(design_size)d.otf-gtable
''' % vars()
    open (path, 'w').write (deps)

    open (os.path.join (outdir, basename + '.fontname'), 'w').write ("%s-%d" % (name, design_size))
