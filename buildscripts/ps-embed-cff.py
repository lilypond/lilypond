#!@PYTHON@
import sys

file  = sys.argv[1]
font_set_name  = sys.argv[2]
output = sys.argv[3]
body = open (file).read() 
body_length  = len (body)
version = '0'
binary_data = "/%(font_set_name)s %(body_length)d StartData "  % vars() \
  + body
binary_length = len (binary_data)

header = r"""%%%%BeginResource: font %(font_set_name)s
%%!PS-Adobe-3.0 Resource-FontSet
%%%%DocumentNeededResources: ProcSet (FontSetInit)
%%%%Title: (FontSet/%(font_set_name)s)
%%%%Version: %(version)s
%%%%EndComments
%%%%IncludeResource: ProcSet (FontSetInit)
%%%%BeginResource: FontSet (%(font_set_name)s)
/FontSetInit /ProcSet findresource begin
%%%%BeginData: %(binary_length)d Binary Bytes
""" % vars()


footer = """\n%%%%EndData
%%%%EndResource
%%%%EOF
%%%%EndResource\n""" % vars()

fo = open (output,'w')
fo.write (header)
fo.write (binary_data)
fo.write (footer)
