#!@PYTHON


## make a fonts.scale file.

import re
import sys
import string
ls =  sys.stdin.readline ()
ls = string.split (ls)
print len(ls)
for fn in ls:
    name = re.sub ('\.pf[ab]', '',fn)    
    name = re.sub ('-', ' ',name)

    m = re.search ("([0-9]+)$", name)
    designsize = 'normal'
    if m:
        designsize =  m.group (1)
        name = re.sub ("([0-9]+)$", "", name)
        
    print '%s -lilypond-%s-regular-r-%s--0-0-0-0-p-0-adobe-fontspecific' % (fn, name, designsize)
    
    
