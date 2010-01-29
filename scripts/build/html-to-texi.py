#! @PYTHON@

import re
import sys

header = '''
'''

print header

body = sys.stdin.read ()
body = re.sub ('(?ms)<!--\s*\n*(.*?)FILL-IN(.*?)\s*\n*!?-->', '', body)
body = re.sub ('(?ms)\n*\s*<!--\s*\n*(.*?)\s*\n*!?-->', r'@ignore\n\1\n@end ignore', body)
body = re.sub ('(?ms)<a(?:\s|\n)*href="([^"]*)"\s*>(.*?)</a>', r'@ref{\1,\2}', body)

body = re.sub ('(?ms)<a(?:\s|\n)*name="([^"]*)"\s*> *</a>', r'@node \1 ', body)
body = re.sub ('(?ms)<h1>(.*?)</h1>', r'@chapheading \1', body)
body = re.sub ('(?ms)<h2>(.*?)</h2>', r'@unnumberedsec \1', body)
#body = re.sub ('(?ms)<a(?:\s|\n)*name="([^"]*)"\s*> *</a>', '', body)
#body = re.sub ('(?ms)<h2>(.*?)</h2>', r'@node \1\n@unnumberedsec \1', body)

body = re.sub ('(?ms)<img(?:\s|\n)*[^>]*src="*([^">]*)(.png|.jpeg)"*.*?>', r'@image{\1,,,\2}', body)
body = re.sub ('\n*<br>\s*\n*', r'@*\n', body)
body = re.sub ('\n*<br[^>]*>\s*\n*', r'@*\n', body)
body = re.sub ('(?ms)<em>(.*?)</em>', r'@emph{\1}', body)
body = re.sub ('(?ms)<blockquote>(.*?)</blockquote>', r'@quote{\1}', body)
body = re.sub ('(?ms)<tt>(.*?)</tt>', r'@code{\1}', body)
body = re.sub ('(?ms)<li>(.*?)(</li>)', r'@item\n\1\n', body)
body = re.sub ('(?ms)<li>(.*?)(<li>)', r'@item\n\1\n\2', body)
body = re.sub ('(?ms)<li>(.*?)(<li>)', r'@item\n\1\n\2', body)
body = re.sub ('(?ms)<li>(.*?)(<li>)', r'@item\n\1\n\2', body)
body = re.sub ('(?ms)<li>(.*?)(<li>)', r'@item\n\1\n\2', body)
body = re.sub ('(?ms)<li>(.*?)(<li>)', r'@item\n\1\n\2', body)
body = re.sub ('(?ms)<li>(.*?)(</(?:u|o)l>)', r'@item\n\1\n\2', body)
body = re.sub ('(?ms)<ul>', r'@itemize', body)
body = re.sub ('(?ms)</ul>', r'@end itemize', body)

# added for the bib => bib2html => html2texinfo chain.  -gp
body = re.sub ('(?ms)<HTML>', r'', body)
body = re.sub ('(?ms)<HEAD>(.*?)(</HEAD>)', r'', body)
body = re.sub ('(?ms)<BODY(.*?)>', r'', body)
body = re.sub ('(?ms)</BODY>', r'', body)
body = re.sub ('(?ms)</HTML>', r'', body)

body = re.sub ('(?ms)<DL>', r'@table @emph', body)
body = re.sub ('(?ms)</DL>', r'@end table', body)

body = re.sub ('(?ms)<DT> <A NAME="(.*?)">(.*?)(</DT>)', r'@item \1', body)
body = re.sub ('(?ms)<A(.*?)>', r'', body)
body = re.sub ('(?ms):</A>', r'', body)
body = re.sub ('(?ms)<DD>(.*?)(</DD>)', r'\1', body)

body = re.sub ('(?ms)<STRONG>(.*?)(</STRONG>)', r'@strong{\1}', body)
body = re.sub ('(?ms)<EM>(.*?)(</EM>)', r'@emph{\1}', body)

body = re.sub ('(?ms)&nbsp', r'@tie{}', body)

body = re.sub ('(?ms)\cite{(.*?)}', r'[\1]', body)
body = re.sub ('(?ms)\cite{(.*?)}', r'[\1]', body)

# dangerous rule, but with the spaces should be ok
body = re.sub ('(?ms) {(.*?)} ', r' @q{\1} ', body)

# end stuff added for the bib => bib2html => html2texinfo chain -gp
# the other lines in the file can _probably_ be trimmed.


body = re.sub ('(?ms)<ol>', r'@enumerate POSITIVE-INTEGER', body)
body = re.sub ('(?ms)<ol\s+start="*(.*?)"*>', r'@enumerate \1', body)
body = re.sub ('(?ms)</ol>', r'@end enumerate', body)

body = re.sub ('&ldquo;(.*?)&rdquo;', r'@qq{\1}', body)
body = re.sub ('&auml;', '@"a', body)
body = re.sub ('(?ms)\s*<p>\s*', '\n\n', body)
body = re.sub ('\n*\s*</p>\s*\n*', '\n\n', body)
body = re.sub ('(?ms)</?font\s*.*?>', '', body)
body = re.sub ('(?ms)<code>(.*?)</code>', r'@code{\1}', body)
body = re.sub ('(?ms)<em>(.*?)</em>', r'@emph{\1}', body)
body = re.sub ('(?ms)<b>(.*?)</b>', r'@strong{\1}', body)
body = re.sub ('(?ms)<pre>(.*?)</pre>', r'@verbatim\n\1\n@end verbatim', body)
body = re.sub ('(?ms)<PRE>(.*?)</PRE>', r'@verbatim\n\1\n@end verbatim', body)

body = re.sub ('(?ms)<table>', r'@table asis', body)
body = re.sub ('(?ms)<table [^>]*>', r'@table asis', body)
body = re.sub ('(?ms)</table>', r'@end table', body)
body = re.sub ('(?ms)<tr>(.*?)</tr>', r'@item\n\1\n', body)
body = re.sub ('(?ms)<th>(.*?)</th>', r'@item\n\1\n', body)
body = re.sub ('(?ms)<td[^>]*>(.*?)</td>', r'@tab\n\1\n', body)

#body = re.sub ('(?ms)\s*<p align="(center|left|right)">\s*((?:.|\n)*)\n\n', '@divClass{float-\\1}\n\\2\n@divEnd', body)
body = re.sub ('(?ms)\s*<p align="(center|left|right)"\s*>\s*', '\n\n@divClass{float-\\1}\n@divEnd\n', body)

print body

