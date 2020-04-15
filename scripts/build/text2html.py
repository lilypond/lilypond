import os
import re
import sys


entities = {
    "&" : 'amp',
    "`" : 'apos',
    '>' : 'gt',
    '<' : 'lt',
    '"' : 'quot',
    }

def txt2html (s):
    for i in list(entities.keys ()):
        s = re.sub (i, '\001' + entities[i] + ';', s);
    s = re.sub ('\001', '&', s);
    return s

for a in sys.argv[1:]:
    # hmm, we need: text2html out/foe.txt -> out/foe.html,
    # -o is a bit overkill?
    # outfile = os.path.basename (os.path.splitext(a)[0]) + '.html'
    outfile = os.path.splitext(a)[0] + '.html'

    try:
      os.unlink(outfile)
    except:
      pass

    s = r"""

<html>
<head>
 <META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
</head>

<body><pre>
%s
</pre></body></html>
""" % txt2html (open (a).read ())
    open (outfile, 'w').write (s)


