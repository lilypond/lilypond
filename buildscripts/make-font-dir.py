 
import re
import sys
import string
ls =  sys.stdin.readline ()
ls = string.split (ls)
print len(ls)
for fn in ls:
    name = re.sub ('\.pf[ab]', '',fn)    
    name = re.sub ('-', ' ',name)

    print '%s -misc-%s-regular-r-normal--0-0-0-0-p-0-adobe-fontspecific' % (fn, name)
    
    
