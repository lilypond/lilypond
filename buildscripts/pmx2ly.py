#!@PYTHON@
import string

ls = open ('barsant.pmx').readlines ()
def stripcomment (l):
	return re.sub ('^%.*$', '', l)
	
ls = map (stripcomment, ls)
ls = filter (lambda x: x <> '', ls)

opening = ls[0]
ls = ls[1:]

opening = map (string.atoi, re.split ('[\t ]+', opening))
(nv,noinst,mtrnuml,mtrdenl,mtrnump,mtrdenp,xmtrnum0,isig) = tuple (opening)


opening = ls[0]
ls = ls[1:]
opening = map (string.atoi, re.split ('[\t ]+', opening))
(npages,nsyst,musicsize,fracindent) = tuple (opening)

for l  in ls:
	pass
	
