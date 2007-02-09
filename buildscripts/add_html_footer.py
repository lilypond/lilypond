#!@PYTHON@

"""
Print a nice footer.
"""
import re
import os
import time

import langdefs

# This is to try to make the docball not too big with almost duplicate files
# see process_links()
non_copied_pages = ['Documentation/user/out-www/lilypond-big-page',
                    'Documentation/user/out-www/lilypond-internals-big-page',
                    'Documentation/user/out-www/music-glossary-big-page',
                    'out-www/examples',
                    'Documentation/topdocs/out-www/NEWS',
                    'Documentation/topdocs/out-www/INSTALL',
                    'Documentation/bibliography/out-www/index',
                    'Documentation/bibliography/out-www/engraving',
                    'Documentation/bibliography/out-www/colorado',
                    'Documentation/bibliography/out-www/computer-notation'
                    'Documentation/out-www/THANKS',
                    'Documentation/out-www/DEDICATION',
                    'Documentation/topdocs/out-www/AUTHORS']

header = r"""
"""

footer = r'''
<div style="background-color: #e8ffe8; padding: 2; border: #c0ffc0 1px solid;">
<p>
<font size="-1">
This page is for %(package_name)s-%(package_version)s (%(branch_str)s). <br>
</font>
<address><font size="-1">
Report errors to <a href="%(mail_address_url)s">%(mail_address)s</a>.</font></address>
</p>
</div>
'''

mail_address = 'http://post.gmane.org/post.php?group=gmane.comp.gnu.lilypond.bugs'

header_tag = '<!-- header_tag -->'
footer_tag = '<!-- footer_tag -->'

def _ (s):
    return s

language_available = _ ("Other languages: %s.") % "%(language_menu)s"
browser_language = _ ("Using <A HREF='%s'>automatic language selection</A>.") \
           % "/web/about/browser-language"

LANGUAGES_TEMPLATE = '''\
<P>
 %(language_available)s
 <BR>
 %(browser_language)s
</P>
''' % vars ()


html_re = re.compile ('(.*?)(?:[.]([^/.]*))?[.]html$')
pages_dict = {}

def build_pages_dict (filelist):
    """Build dictionnary of available translations of each page"""
    global pages_dict
    for f in filelist:
        m = html_re.match (f)
        if m:
            g = m.groups()
            if len (g) <= 1 or g[1] == None:
                e = ''
            else:
                e = g[1]
            if not g[0] in pages_dict.keys():
                pages_dict[g[0]] = [e]
            else:
                pages_dict[g[0]].append (e)

def add_header (s):
    """Add header (<BODY> and doctype)"""
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
        return s

def info_external_ref_remove (s):
    """Remove info's annoying's indication of referencing external document"""
    return re.sub (' \((lilypond|lilypond-internals|music-glossary)\)</a>', '</a>', s)

def add_title (s):
    # urg
    # maybe find first node?
    fallback_web_title = '-- --'
    m = re.match ('.*?<title>(.*?)</title>', s, re.DOTALL)
    if m:
        fallback_web_title = m.group (1)
    s = re.sub ('@WEB-TITLE@', fallback_web_title, s)
    return s

info_nav_bar = re.compile (r'<div class="node">\s*<p>\s*<a name=".+?"></a>(.+?)<hr>\s*</div>', re.M | re.S)

def add_footer (s):
    """add footer

also add navigation bar to bottom of Info HTML pages"""
    m = info_nav_bar.search (s)
    if m:
        custom_footer = '<br><hr>\n<div class="node">\n<p>' + m.group (1) + '</div>\n' + footer
    else:
        custom_footer = footer
    if re.search ('(?i)</body', s):
        s = re.sub ('(?i)</body>', footer_tag + custom_footer + '\n' + '</BODY>', s, 1)
    elif re.search ('(?i)</html', s):                
        s = re.sub ('(?i)</html>', footer_tag + custom_footer + '\n' + '</HTML>', s, 1)
    else:
        s += footer_tag + custom_footer + '\n'
    return s

def find_translations (prefix, lang_ext):
    """find available translations of a page"""
    available = []
    missing = []
    for l in langdefs.LANGUAGES:
        e = l.webext
        if lang_ext != e:
            if e in pages_dict[prefix]:
                available.append (l)
            elif lang_ext == '' and l.enabled and not prefix in non_copied_pages:
                # English version of missing translated pages will be written
                missing.append (e)
    return available, missing

def process_links (s, prefix, lang_ext, file_name, missing, target):
    page_flavors = {}
    if target == 'online':
        # Strip .html, .png suffix for auto language selection (content
        # negotiation).  The menu must keep the full extension, so do
        # this before adding the menu.
        page_flavors[file_name] = re.sub (
            '''(href|src)=[\'"]([^/][.]*[^.:\'"]*)(.html|.png)(#[^"\']*|)[\'"]''',
            '\\1="\\2\\4"', s)
    elif target == 'offline':
        # in LANG doc index: don't rewrite .html suffixes as not all .LANG.html pages exist
        # the doc index should be translated and contain the right links
        if prefix == 'Documentation/out-www/index':
            page_flavors[file_name] = s
        elif lang_ext == '':
            page_flavors[file_name] = s
            for e in missing:
                page_flavors[langdefs.lang_file_name (prefix, e, '.html')] = re.sub (
                    '''href=[\'"]([^/][.]*[^.:\'"]*)(.html)(#[^"\']*|)[\'"]''',
                    'href="\\1.' + e + '\\2\\3"', s)
        else:
            page_flavors[file_name] = re.sub (
                '''href=[\'"]([^/][.]*[^.:\'"]*)(.html)(#[^"\']*|)[\'"]''',
                'href="\\1.' + lang_ext + '\\2\\3"', s)
    return page_flavors

def add_menu (page_flavors, prefix, available):
    language_menu = ''
    for lang in available:
        lang_file = lang.file_name (os.path.basename (prefix), '.html')
        if language_menu != '':
            language_menu += ', '
        language_menu += '<a href="%s">%s</a>' % (lang_file, lang.name)

    languages = ''
    if language_menu:
        languages = LANGUAGES_TEMPLATE % vars ()

    # put language menu before '</body>' and '</html>' tags
    for k in page_flavors.keys():
        if re.search ('(?i)</body', page_flavors[k]):
            page_flavors[k] = re.sub ('(?i)</body>', languages + '</BODY>', page_flavors[k], 1)
        elif re.search ('(?i)</html', page_flavors[k]):                
            page_flavors[k] = re.sub ('(?i)</html>', languages + '</HTML>', page_flavors[k], 1)
        else:
            page_flavors[k] += languages
    return page_flavors


def add_html_footer (package_name = '',
                     package_version = '',
                     target = 'offline',
                     name_filter = lambda s: s):
    """Add header, footer to a number of HTML files

    Arguments:
     package_name=NAME         set package_name to NAME
     package_version=VERSION   set package version to VERSION
     targets=offline|online    set page processing depending on the target
          offline is for reading HTML pages locally
          online is for hosting the HTML pages on a website with content
            negotiation
     name_filter               a HTML file name filter
    """
    localtime = time.strftime ('%c %Z', time.localtime (time.time ()))

    if re.search ("http://", mail_address):
        mail_address_url = mail_address
    else:
        mail_address_url= 'mailto:' + mail_address

    versiontup = package_version.split ('.')
    branch_str = 'stable-branch'
    if int ( versiontup[1]) %  2:
        branch_str = 'development-branch'

    for prefix, ext_list in pages_dict.items ():
        for lang_ext in ext_list:
            file_name = langdefs.lang_file_name (prefix, lang_ext, '.html')
            in_f = open (file_name)
            s = in_f.read()
            in_f.close()

            s = re.sub ('%', '%%', s)
            s = add_header (s)
            # seems to be no more needed
            # s = info_external_ref_remove (s)

            ### add footer
            if re.search (footer_tag, s) == None:
                s = add_footer (s)
                available, missing = find_translations (prefix, lang_ext)
                page_flavors = process_links (s, prefix, lang_ext, file_name, missing, target)
                # Add menu after stripping: must not have autoselection for language menu.
                page_flavors = add_menu (page_flavors, prefix, available)
            # urg, this stuff is oudated and seems useless, let's disable it
            #else:
            #    for e in [l.webext for l in langdefs.LANGUAGES]:
            #        if not e in pages_dict[prefix]:
            #            page_flavors[langdefs.lang_file_name (prefix, e, '.html')] = s

            subst = globals ()
            subst.update (locals())
            for k in page_flavors.keys():
                page_flavors[k] = page_flavors[k] % subst

                out_f = open (name_filter (k), 'w')
                out_f.write (page_flavors[k])
                out_f.close()
        # if the page is translated, a .en.html symlink is necessary for content negotiation
        if target == 'online' and ext_list != ['']:
            os.symlink (os.path.basename (prefix) + '.html', name_filter (prefix + '.en.html'))
