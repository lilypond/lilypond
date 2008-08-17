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
                    'Documentation/user/out-www/lilypond-learning-big-page',
                    'Documentation/user/out-www/lilypond-program-big-page',
                    'Documentation/user/out-www/music-glossary-big-page',
                    'out-www/examples',
                    'Documentation/topdocs',
                    'Documentation/bibliography',
                    'Documentation/out-www/THANKS',
                    'Documentation/out-www/DEDICATION',
                    'Documentation/out-www/devel',
                    'input/']

def _doc (s):
    return s

header = r"""
"""

footer = '''
<div style="background-color: #e8ffe8; padding: 2; border: #c0ffc0 1px solid;">
<p>
<font size="-1">
%(footer_name_version)s
<br>
<address>
%(footer_report_errors)s </address>
<br>
%(footer_suggest_docs)s
</font>
</p>
</div>
'''
footer_name_version = _doc ('This page is for %(package_name)s-%(package_version)s (%(branch_str)s).')
footer_report_errors = _doc ('Report errors to <a href="%(mail_address_url)s">%(mail_address)s</a>.')
# ugh, must not have "_doc" in strings because it is naively replaced with "_" in hacked gettext process
footer_suggest_docs = _doc ('Your <a href="%(suggest_Docs_url)s">suggestions for the documentation</a> are welcome.')

mail_address = 'http://post.gmane.org/post.php?group=gmane.comp.gnu.lilypond.bugs'
suggest_Docs_url = 'http://lilypond.org/web/devel/participating/documentation-adding'

header_tag = '<!-- header_tag -->'
footer_tag = '<!-- footer_tag -->'

lang_available = _doc ("Other languages: %s.")
browser_lang = _doc ('About <A HREF="%s">automatic language selection</A>.')
browser_language_url = "/web/about/browser-language"

LANGUAGES_TEMPLATE = '''
<P>
 %(language_available)s
 <BR>
 %(browser_language)s
</P>
'''


html_re = re.compile ('(.*?)(?:[.]([^/.]*))?[.]html$')
pages_dict = {}

def build_pages_dict (filelist):
    """Build dictionary of available translations of each page"""
    global pages_dict
    for f in filelist:
        m = html_re.match (f)
        if m:
            g = m.groups()
            if len (g) <= 1 or g[1] == None:
                e = ''
            else:
                e = g[1]
            if not g[0] in pages_dict:
                pages_dict[g[0]] = [e]
            else:
                pages_dict[g[0]].append (e)

def source_links_replace (m, source_val):
    return 'href="' + os.path.join (source_val, m.group (1)) + '"'

splitted_docs_re = re.compile ('(input/lsr/out-www/lilypond-snippets|Documentation/user/out-www/(lilypond|music-glossary|lilypond-program|lilypond-learning))/')

snippets_ref_re = re.compile (r'href="(\.\./)?lilypond-snippets')
user_ref_re = re.compile (r'href="(?:\.\./)?lilypond(|-internals|-learning|-program)')

## Windows does not support symlinks.
# This function avoids creating symlinks for splitted HTML manuals
# Get rid of symlinks in GNUmakefile.in (local-WWW-post)
# this also fixes missing PNGs only present in translated docs
def hack_urls (s, prefix):
    if splitted_docs_re.match (prefix):
        s = re.sub ('(href|src)="(../lily-.*?|.*?[.]png)"', '\\1="../\\2"', s)

    # fix xrefs between documents in different directories ad hoc
    if 'user/out-www/lilypond' in prefix:
        s = snippets_ref_re.sub ('href="source/input/lsr/lilypond-snippets', s)
    elif 'input/lsr' in prefix:
        s = user_ref_re.sub ('href="source/Documentation/user/lilypond\\1', s)

    source_path = os.path.join (os.path.dirname (prefix), 'source')
    if not os.path.islink (source_path):
        return s
    source_val = os.readlink (source_path)
    return re.sub ('href="source/(.*?)"', lambda m: source_links_replace (m, source_val), s)

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
info_footnote_hr = re.compile (r'<hr>\s*(</div>)?\s*</body>', re.M | re.I)

def add_footer (s):
    """add footer

also add navigation bar to bottom of Info HTML pages"""
    m = info_nav_bar.search (s)
    if m:
        # avoid duplicate <hr> in case there are footnotes at the end of the Info HTML page
        if info_footnote_hr.search (s):
            custom_footer = '<div class="node">\n<p>' + m.group (1) + '</div>\n' + footer
        else:
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
            elif lang_ext == '' and l.enabled and reduce (lambda x, y: x and y, [not prefix.startswith (s) for s in non_copied_pages]):
                # English version of missing translated pages will be written
                missing.append (e)
    return available, missing

def process_links (s, prefix, lang_ext, file_name, missing, target):
    page_flavors = {}
    if target == 'online':
        # Strip .html, .png suffix for auto language selection (content
        # negotiation).  The menu must keep the full extension, so do
        # this before adding the menu.
        page_flavors[file_name] = [lang_ext, re.sub (
            '''(href|src)=[\'"]([^/][.]*[^.:\'"]*)(.html|.png)(#[^"\']*|)[\'"]''',
            '\\1="\\2\\4"', s)]
    elif target == 'offline':
        # in LANG doc index: don't rewrite .html suffixes as not all .LANG.html pages exist
        # the doc index should be translated and contain the right links
        if prefix == 'Documentation/out-www/index':
            page_flavors[file_name] = [lang_ext, s]
        elif lang_ext == '':
            page_flavors[file_name] = [lang_ext, s]
            for e in missing:
                page_flavors[langdefs.lang_file_name (prefix, e, '.html')] = [e, re.sub (
                    '''href=[\'"]([^/][.]*[^.:\'"]*)(.html)(#[^"\']*|)[\'"]''',
                    'href="\\1.' + e + '\\2\\3"', s)]
        else:
            page_flavors[file_name] = [lang_ext, re.sub (
                '''href=[\'"]([^/][.]*[^.:\'"]*)(.html)(#[^"\']*|)[\'"]''',
                'href="\\1.' + lang_ext + '\\2\\3"', s)]
    return page_flavors

def add_menu (page_flavors, prefix, available, target, translation):
    for k in page_flavors:
        language_menu = ''
        languages = ''
        if page_flavors[k][0] != '':
            t = translation[page_flavors[k][0]]
        else:
            t = _doc
        for lang in available:
            lang_file = lang.file_name (os.path.basename (prefix), '.html')
            if language_menu != '':
                language_menu += ', '
            language_menu += '<a href="%s">%s</a>' % (lang_file, t (lang.name))
        if target == 'offline':
            browser_language = ''
        elif target == 'online':
            browser_language = t (browser_lang) % browser_language_url
        if language_menu:
            language_available = t (lang_available) % language_menu
            languages = LANGUAGES_TEMPLATE % vars ()
        # put language menu before '</body>' and '</html>' tags
        if re.search ('(?i)</body', page_flavors[k][1]):
            page_flavors[k][1] = re.sub ('(?i)</body>', languages + '</BODY>', page_flavors[k][1], 1)
        elif re.search ('(?i)</html', page_flavors[k][1]):
            page_flavors[k][1] = re.sub ('(?i)</html>', languages + '</HTML>', page_flavors[k][1], 1)
        else:
            page_flavors[k][1] += languages
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
    translation = langdefs.translation
    localtime = time.strftime ('%c %Z', time.localtime (time.time ()))

    if "http://" in mail_address:
        mail_address_url = mail_address
    else:
        mail_address_url= 'mailto:' + mail_address

    versiontup = package_version.split ('.')
    branch_str = _doc ('stable-branch')
    if int (versiontup[1]) %  2:
        branch_str = _doc ('development-branch')

    # Initialize dictionaries for string formatting
    subst = {}
    subst[''] = dict ([i for i in globals ().items() if type (i[1]) is str])
    subst[''].update (dict ([i for i in locals ().items() if type (i[1]) is str]))
    for l in translation:
        e = langdefs.LANGDICT[l].webext
        if e:
            subst[e] = {}
            for name in subst['']:
                subst[e][name] = translation[l] (subst[''][name])
    # Do deeper string formatting as early as possible,
    # so only one '%' formatting pass is needed later
    for e in subst:
        subst[e]['footer_name_version'] = subst[e]['footer_name_version'] % subst[e]
        subst[e]['footer_report_errors'] = subst[e]['footer_report_errors'] % subst[e]
        subst[e]['footer_suggest_docs'] = subst[e]['footer_suggest_docs'] % subst[e]

    for prefix, ext_list in pages_dict.items ():
        for lang_ext in ext_list:
            file_name = langdefs.lang_file_name (prefix, lang_ext, '.html')
            in_f = open (file_name)
            s = in_f.read()
            in_f.close()

            s = re.sub ('%', '%%', s)
            s = hack_urls (s, prefix)
            s = add_header (s)

            ### add footer
            if re.search (footer_tag, s) == None:
                s = add_footer (s)
                
                available, missing = find_translations (prefix, lang_ext)
                page_flavors = process_links (s, prefix, lang_ext, file_name, missing, target)
                # Add menu after stripping: must not have autoselection for language menu.
                page_flavors = add_menu (page_flavors, prefix, available, target, translation)
            for k in page_flavors:
                page_flavors[k][1] = page_flavors[k][1] % subst[page_flavors[k][0]]
                out_f = open (name_filter (k), 'w')
                out_f.write (page_flavors[k][1])
                out_f.close()
        # if the page is translated, a .en.html symlink is necessary for content negotiation
        if target == 'online' and ext_list != ['']:
            os.symlink (os.path.basename (prefix) + '.html', name_filter (prefix + '.en.html'))
