#!@PYTHON@
import os
import sys
import tempfile

base = os.path.splitext (os.path.split (sys.argv[1])[1])[0]
input = os.path.abspath (sys.argv[1])
output = os.path.abspath (sys.argv[2])
program_name= os.path.split (sys.argv[0])[1]

dir = tempfile.mktemp (program_name)
os.mkdir (dir, 0777)
os.chdir(dir)

def system (c):
    print c
    if os.system (c):
        raise 'barf'

outputs = []
for sz in [48,32,16] :
    
    for depth in [24,8]:
        out = '%(base)s-%(sz)d-%(depth)d.png' % locals()
        system ('convert -depth %(depth)d -sample %(sz)d %(input)s %(out)s' %
            locals ())
        outputs.append (out)
        
system('icotool --output %s --create %s' % (output, ' '.join (outputs)))
system('rm -rf %(dir)s' % locals())                 

