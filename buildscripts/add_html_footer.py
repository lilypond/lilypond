#!@PYTHON@

"""
Print a nice footer.
"""
import re
import os
import time

import langdefs

default_header = r"""
"""

default_footer = r'''
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

def build_pages_dict (filelist):
    """Build dictionnary of available translations of each page"""
    pages_dict = {}
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
    return pages_dict


def do_file (prefix, lang_ext, target, header, footer, pages_dict, out_root, name_filter,
             package_name, package_version, branch_str, mail_address_url, mail_address):
    file_name = langdefs.lang_file_name (prefix, lang_ext, '.html')
    in_f = open (file_name)
    s = in_f.read()
    in_f.close()
    
    s = re.sub ('%', '%%', s)

    ### add header
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

    # remove info's annoying's indication of referencing external document
    s = re.sub (' \((lilypond|lilypond-internals|music-glossary)\)</a>',                                  '</a>', s)

    # urg
    # maybe find first node?
    fallback_web_title = '-- --'
    m = re.match ('.*?<title>(.*?)</title>', s, re.DOTALL)
    if m:
        fallback_web_title = m.group (1)
    s = re.sub ('@WEB-TITLE@', fallback_web_title, s)

    ### add footer
    page_flavors = {}
    if re.search (footer_tag, s) == None:
        if re.search ('(?i)</body', s):
            s = re.sub ('(?i)</body>', footer_tag + footer + '\n' + '</BODY>', s, 1)
        elif re.search ('(?i)</html', s):                
            s = re.sub ('(?i)</html>', footer_tag + footer + '\n' + '</HTML>', s, 1)
        else:
            s += footer_tag + footer + '\n'
        
        # Find available translations of this page.
        available = []
        missing = []
        for l in langdefs.LANGUAGES:
            e = l.webext
            if lang_ext != e:
                if e in pages_dict[prefix]:
                    available.append (l)
                elif lang_ext == '' and l.enabled: # English version of missing translated pages will be written
                    missing.append (e)

        if target == 'online':
            # Strip .html, .png suffix for auto language selection (content
            # negotiation).  The menu must keep the full extension, so do
            # this before adding the menu.
            # Don't strip .html suffix for documentation index because of
            # lilypond/ vs. lilypond.html conflict
            if prefix == 'Documentation/out-www/index':
                page_flavors[file_name] = s
            else:
                page_flavors[file_name] = re.sub (
                    '''(href|src)=[\'"]([^/][.]*[^.:\'"]*)(.html|.png)(#[^"\']*|)[\'"]''',
                    '\\1="\\2\\4"', s)
        elif target == 'offline':
            if lang_ext == '':
                page_flavors[file_name] = s
                for e in missing:
                    page_flavors[langdefs.lang_file_name (prefix, e, '.html')] = re.sub (
                        '''href=[\'"]([^/][.]*[^.:\'"]*)(.html)(#[^"\']*|)[\'"]''',
                        'href="\\1.' + e + '\\2\\3"', s)
            else:
                page_flavors[file_name] = re.sub (
                    '''href=[\'"]([^/][.]*[^.:\'"]*)(.html)(#[^"\']*|)[\'"]''',
                    'href="\\1.' + lang_ext + '\\2\\3"', s)

        # Add menu after stripping: must not have autoselection for language menu.
        language_menu = ''
        for lang in available:
            lang_file = lang.file_name (os.path.basename (prefix), '.html')
            if language_menu != '':
                language_menu += ', '
            language_menu += '<a href="%s">%s</a>' % (lang_file, lang.name)

        languages = ''
        if language_menu:
            languages = LANGUAGES_TEMPLATE % vars ()

        # Put language menu before '</body>' and '</html>' tags
        for k in page_flavors.keys():
            if re.search ('(?i)</body', page_flavors[k]):
                page_flavors[k] = re.sub ('(?i)</body>', languages + '</BODY>', page_flavors[k], 1)
            elif re.search ('(?i)</html', page_flavors[k]):                
                page_flavors[k] = re.sub ('(?i)</html>', languages + '</HTML>', page_flavors[k], 1)
            else:
                page_flavors[k] += languages
    else:
        for e in [l.webext for l in langdefs.LANGUAGES]:
            if not e in pages_dict[prefix]:
                page_flavors[langdefs.lang_file_name (prefix, e, '.html')] = s

    for k in page_flavors.keys():
        page_flavors[k] = page_flavors[k] % vars ()

        out_f = open (os.path.join (out_root, name_filter (k)), 'w')
        out_f.write (page_flavors[k])
        out_f.close()


def add_html_footer (package_name = '',
                     package_version = '',
                     header = default_header,
                     footer = default_footer,
                     target = 'offline',
                     mail_address = '(address unknown)',
                     pages_dict = {},
                     out_root = '',
                     name_filter = lambda s: s):
    """Add header, footer to a number of HTML files

    Arguments:
     package_name=NAME         set package_name to NAME
     package_version=VERSION   set package version to VERSION
     header=TEXT               use TEXT as header
     footer=TEXT               use TEXT as footer
     targets=offline|online    set page processing depending on the target
          offline is for reading HTML pages locally
          online is for hosting the HTML pages on a website with content
            negotiation
     mail_address              set \"Report errors to\" link
     pages_dict                a dictionnary returned by build_pages_dict()
     out_root                  a path prefix where to write HTML pages
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

    for page, ext_list in pages_dict.items ():
        for e in ext_list:
            do_file (page, e, target, header, footer, pages_dict, out_root, name_filter,
                     package_name, package_version, branch_str, mail_address_url, mail_address)
        # if the page is translated, a .en.html symlink is necessary for content negotiation
        if target == 'online' and ext_list != ['']:
            os.symlink (os.path.basename (page) + '.html', os.path.join (out_root, name_filter (page + '.en.html')))
