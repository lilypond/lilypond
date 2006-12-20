#!@PYTHON@

"""
Print a nice footer.  add the top of the ChangeLog file (up to the ********)
"""
import re
import sys
import os
import time
import string 
import getopt

index_url=''
top_url=''
changelog_file=''
content_negotiation = False
package_name = ''
package_version = ''

mail_address = '(address unknown)'
try:
    mail_address= os.environ['MAILADDRESS']
except KeyError:
    pass

mail_address_url= 'mailto:' + mail_address
if re.search ("http://", mail_address):
    mail_address_url = mail_address
    
webmaster= mail_address
try:
    webmaster= os.environ['WEBMASTER']
except KeyError:
    pass

header_file = ''
footer_file = ''
default_header = r"""
"""


#wiki_base = 'http://afavant.elte.hu/lywiki/'
wiki_base = None 


default_footer = r"""<hr>Please take me <a href=@INDEX@>back to the index</a>
of @PACKAGE_NAME@
"""

built = r'''
<div style="background-color: #e8ffe8; padding: 2; border: #c0ffc0 1px solid;">
%(wiki_string)s
<p>
<font size="-1">
This page is for %(package_name)s-%(package_version)s (%(branch_str)s). <br>
</font>
<address><font size="-1">
Report errors to <a href="%(mail_address_url)s">%(mail_address)s</a>.</font></address>
</p>
</div>
'''




def help ():
    sys.stdout.write (r"""Usage: add-html-footer [OPTIONS]... HTML-FILE
Add header, footer and top of ChangLog file (up to the ********) to HTML-FILE

Options:
 --changelog=FILE          use FILE as ChangeLog [ChangeLog]
 --content-negotiation     strip .html and .png from urls
 --footer=FILE             use FILE as footer
 --header=FILE             use FILE as header
 -h, --help                print this help
 --index=URL               set homepage to URL
 --name=NAME               set package_name to NAME
 --version=VERSION         set package version to VERSION

""")
    sys.exit (0)

(options, files) = getopt.getopt(sys.argv[1:], 'h', [
    'changelog=', 'footer=', 'header=', 'help', 'index=',
    'name=', 'content-negotiation', 'version=']) 

for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '--changelog':
        changelog_file = a
    elif o == '--content-negotiation':
        content_negotiation = True
    elif o == '--footer':
        footer_file = a
    elif o == '--header':
        header_file = a
    elif o == '-h' or o == '--help':
        help ()
    # urg, this is top!
    elif o == '--index':
        index_url = a
    elif o == '--name':
        package_name = a
    elif o == '--version':
        package_version = a
    else:
        raise 'unknown opt ', o


def compose (default, file):
    s = default
    if file:
        s = open (file).read ()
    return s

localtime = time.strftime ('%c %Z', time.localtime (time.time ()))

if os.path.basename (index_url) != "index.html":
    index_url = os.path.join (index_url , "index.html")
top_url = os.path.dirname (index_url) + "/"

header = compose (default_header, header_file)

# compose (default_footer, footer_file)
footer =  built
header_tag = '<!-- header_tag -->'
footer_tag = '<!-- footer_tag -->'

# Python < 1.5.2 compatibility
#
# On most platforms, this is equivalent to
#`normpath(join(os.getcwd()), PATH)'.  *Added in Python version 1.5.2*
if os.path.__dict__.has_key ('abspath'):
    abspath = os.path.abspath
else:
    def abspath (path):
        return os.path.normpath (os.path.join (os.getcwd (), path))


def remove_self_ref (s):        
    self_url = abspath (os.getcwd () + '/' + f)
    #sys.stderr.write ('url0: %s\n' % self_url)

    # self_url = re.sub ('.*?' + string.lower (package_name) + '[^/]*/',
    #                 '', self_url)
    # URG - this only works when source tree is unpacked in `src/' dir
    # For some reason, .*? still eats away
    #     /home/fred/usr/src/lilypond-1.5.14/Documentation/user/out-www/lilypond/
    # instead of just
    #
    #     /home/fred/usr/src/lilypond-1.5.14/
    #
    #     Tutorial.html
    self_url = re.sub ('.*?src/' + string.lower (package_name) + '[^/]*/',
            '', self_url)

    #sys.stderr.write ('url1: %s\n' % self_url)
    
    #urg, ugly lily-specific toplevel index hack
    self_url = re.sub ('.*topdocs/out-www/index.html', 'index.html', self_url)
    #sys.stderr.write ('url2: %s\n' % self_url)

    # ugh, python2.[12] re is broken.
    ## pat = re.compile ('.*?(<a href="[\./]*' + self_url + '#?[^"]*">)([^<]*)(</a>)', re.DOTALL)
    pat = re.compile ('[.\n]*?(<a href="[\./]*' + self_url + '#?[^"]*">)([^<]*)(</a>)')
    m = pat.search (s)
    while m:
        #sys.stderr.write ('self: %s\n' % m.group (2))
        s = s[:m.start (1)] + m.group (2) + s[m.end (3):]
        m = pat.search (s)
    return s

def do_file (f):
    if os.path.islink (f):
        return
    
    s = open (f).read()
    s = re.sub ('%', '%%', s)


    if re.search (header_tag, s) == None:
        body = '<BODY BGCOLOR=WHITE TEXT=BLACK>'
        s = re.sub ('(?i)<body>', body, s)
        if re.search ('(?i)<BODY', s):
            s = re.sub ('(?i)<body[^>]*>', body + header, s, 1)
        elif re.search ('(?i)<html', s):                
            s = re.sub ('(?i)<html>', '<HTML>' + header, s, 1)
        else:
            s = header + s

        s = header_tag + '\n' + s

        if re.search ('(?i)<!DOCTYPE', s) == None:
            doctype = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n'
            s = doctype + s

    if re.search (footer_tag, s) == None:
        if re.search ('(?i)</body', s):
            s = re.sub ('(?i)</body>', footer_tag + footer + '\n' + '</BODY>', s, 1)
        elif re.search ('(?i)</html', s):                
            s = re.sub ('(?i)</html>', footer_tag + footer + '\n' + '</HTML>', s, 1)
        else:
            s = s + footer_tag + footer + '\n'

        s = i18n (f, s)

    #URUGRGOUSNGUOUNRIU
    index = index_url
    top = top_url
    if os.path.basename (f) == "index.html":
        cwd = os.getcwd ()
        if os.path.basename (cwd) == "topdocs":
            index = "index.html"
            top = ""

        # don't cause ///////index.html entries in log files.
        #        index = "./index.html"
        #        top = "./"
            
    versiontup = string.split(package_version, '.')
    branch_str = 'stable-branch'
    if string.atoi ( versiontup[1]) %  2:
        branch_str = 'development-branch'

    wiki_page = ('v%s.%s-' % (versiontup[0], versiontup[1]) +  f)
    wiki_page = re.sub ('out-www/', '', wiki_page)
    wiki_page = re.sub ('/', '-', wiki_page) 
    wiki_page = re.sub (r'\.-', '', wiki_page) 
    wiki_page = re.sub ('.html', '', wiki_page)

    wiki_string = ''

    if wiki_base:
        wiki_string = (r'''<a href="%(wiki_base)s%(wiki_page)s">Read </a> comments on this page, or
        <a href="%(wiki_base)s%(wiki_page)s?action=edit">add</a> one.''' % 
               { 'wiki_base': wiki_base,
                'wiki_page': wiki_page})
        
    subst = globals ()
    subst.update (locals())
    s = s % subst

    # urg
    # maybe find first node?
    fallback_web_title = '-- --'

    # ugh, python2.[12] re is broken.
    #m = re.match ('.*?<title>\(.*?\)</title>', s, re.DOTALL)
    m = re.match ('[.\n]*?<title>([.\n]*?)</title>', s)
    if m:
        fallback_web_title = m.group (1)
    s = re.sub ('@WEB-TITLE@', fallback_web_title, s)
    
    s = remove_self_ref (s)

    # remove info's annoying's indication of referencing external document
    s = re.sub (' \((lilypond|lilypond-internals|music-glossary)\)</a>',
          '</a>', s)

    if not os.path.islink (f):
        open (f, 'w').write (s)



localedir = 'out/locale'
try:
    import gettext
    gettext.bindtextdomain ('newweb', localedir)
    gettext.textdomain ('newweb')
    _ = gettext.gettext
except:
    def _ (s):
        return s
underscore = _

C = 'site'
LANGUAGES = (
    (C, 'English'),
    ('nl', 'Nederlands'),
    ('fr', 'French')
    )

language_available = _ ("Other languages: %s.") % "%(language_menu)s"
browser_language = _ ("Using <A HREF='%s'>automatic language selection</A>.") \
           % "%(root_url)sabout/browser-language"

LANGUAGES_TEMPLATE = '''\
<P>
 %(language_available)s
 <BR>
 %(browser_language)s
</P>
''' % vars ()

def file_lang (file, lang):
    (base, ext) = os.path.splitext (file)
    base = os.path.splitext (base)[0]
    if lang and lang != C:
        return base + '.' + lang + ext
    return base + ext


def i18n (file_name, page):
    # ugh
    root_url = "/web/"

    base_name = os.path.basename (file_name)

    lang = C
    m = re.match ('.*[.]([^/.]*).html', file_name)
    if m:
        lang = m.group (1)

    # Find available translations of this page.
    available = filter (lambda x: lang != x[0] \
              and os.path.exists (file_lang (file_name, x[0])),
              LANGUAGES)

    # Strip .html, .png suffix for auto language selection (content
    # negotiation).  The menu must keep the full extension, so do
    # this before adding the menu.
    if content_negotiation:
        page = re.sub ('''(href|src)=[\'"]([^/][.]*[^.:\'"]*)(.html|.png)(#[^"\']*|)[\'"]''',
                       '\\1="\\2\\4"', page)

    # Add menu after stripping: must not have autoselection for language menu.
    language_menu = ''
    for (prefix, name) in available:
        lang_file = file_lang (base_name, prefix)
        if language_menu != '':
            language_menu += ', '
        language_menu += '<a href="%(lang_file)s">%(name)s</a>' % vars ()

    languages = ''
    if language_menu:
        languages = LANGUAGES_TEMPLATE % vars ()

    # Put language menu before '</body>' and '</html>' tags
    if re.search ('(?i)</body', page):
        page = re.sub ('(?i)</body>', languages + '</BODY>', page, 1)
    elif re.search ('(?i)</html', page):                
        page = re.sub ('(?i)</html>', languages + '</HTML>', page, 1)
    else:
        page = page + languages

    if content_negotiation and language_menu:
        os.symlink (file_name, os.path.splitext (os.path.basename (file_name))[0] + '.en.html')
        
    return page

for f in files:
    do_file (f)

