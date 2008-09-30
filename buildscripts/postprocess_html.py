#!@PYTHON@

"""
Postprocess HTML files:
add footer, tweak links, add language selection menu.
"""
import re
import os
import time
import operator

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
<div class="footer">
<p class="footer_version">
%(footer_name_version)s
</p>
<p class="footer_report">
%(footer_report_links)s
</p>
</div>
'''
footer_name_version = _doc ('This page is for %(package_name)s-%(package_version)s (%(branch_str)s).')
# ugh, must not have "_doc" in strings because it is naively replaced with "_" in hacked gettext process
footer_report_links = _doc ('Your <a href="%(suggest_Docs_url)s">suggestions for the documentation</a> are welcome, please report errors to our <a href="%(mail_address_url)s">bug list</a>.')


mail_address = 'http://post.gmane.org/post.php?group=gmane.comp.gnu.lilypond.bugs'
suggest_Docs_url = 'http://lilypond.org/web/devel/participating/documentation-adding'

header_tag = '<!-- header_tag -->'
header_tag_re = re.compile (header_tag)

footer_tag = '<!-- footer_tag -->'
footer_tag_re = re.compile (footer_tag)

lang_available = _doc ("Other languages: %s.")
browser_lang = _doc ('About <A HREF="%s">automatic language selection</A>.')
browser_language_url = "/web/about/browser-language"

LANGUAGES_TEMPLATE = '''
<p id="languages">
 %(language_available)s
 <br/>
 %(browser_language)s
</p>
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

splitted_docs_re = re.compile ('(input/lsr/out-www/lilypond-snippets|\
Documentation/user/out-www/(lilypond|music-glossary|lilypond-program|\
lilypond-learning))/')

snippets_ref_re = re.compile (r'href="(\.\./)?lilypond-snippets')
user_ref_re = re.compile ('href="(?:\.\./)?lilypond\
(-internals|-learning|-program|(?!-snippets))')

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

body_tag_re = re.compile ('(?i)<body([^>]*)>')
html_tag_re = re.compile ('(?i)<html>')
doctype_re = re.compile ('(?i)<!DOCTYPE')
doctype = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n'
css_re = re.compile ('(?i)<link rel="stylesheet" type="text/css" ([^>]*)href="[^">]*?lilypond.css"([^>]*)>')
end_head_tag_re = re.compile ('(?i)</head>')
css_link = '<link rel="stylesheet" type="text/css" href="%sDocumentation/lilypond.css">\n'

def add_header (s, prefix):
    """Add header (<body>, doctype and CSS)"""
    if header_tag_re.search (s) == None:
        body = '<body\\1>'
        (s, n) = body_tag_re.subn (body + header, s, 1)
        if not n:
            (s, n) = html_tag_re.subn ('<html>' + header, s, 1)
            if not n:
                s = header + s

        s = header_tag + '\n' + s

        if doctype_re.search (s) == None:
            s = doctype + s

        if css_re.search (s) == None:
            depth = (prefix.count ('/') - 1) * '../'
            s = end_head_tag_re.sub ((css_link % depth) + '</head>', s)
    return s

title_tag_re = re.compile ('.*?<title>(.*?)</title>', re.DOTALL)
AT_web_title_re = re.compile ('@WEB-TITLE@')

def add_title (s):
    # urg
    # maybe find first node?
    fallback_web_title = '-- --'
    m = title_tag_re.match (s)
    if m:
        fallback_web_title = m.group (1)
    s = AT_web_title_re.sub (fallback_web_title, s)
    return s

footer_insert_re = re.compile ('<!--\s*FOOTER\s*-->')
end_body_re = re.compile ('(?i)</body>')
end_html_re = re.compile ('(?i)</html>')

def add_footer (s, footer_text):
    """add footer"""
    (s, n) = footer_insert_re.subn (footer_text + '\n' + '<!-- FOOTER -->', s, 1)
    if not n:
        (s, n) = end_body_re.subn (footer_text + '\n' + '</body>', s, 1)
    if not n:
        (s, n) = end_html_re.subn (footer_text + '\n' + '</html>', s, 1)
    if not n:
        s += footer_text + '\n'
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
            elif lang_ext == '' and l.enabled and reduce (operator.and_,
                                                          [not prefix.startswith (s)
                                                           for s in non_copied_pages]):
                # English version of missing translated pages will be written
                missing.append (e)
    return available, missing

online_links_re = re.compile ('''(href|src)=['"]\
((?!Compiling-from-source.html")[^/][.]*[^.:'"]*)\
([.]html)(#[^"']*|)['"]''')
offline_links_re = re.compile ('href=[\'"]\
((?!Compiling-from-source.html")[^/][.]*[^.:\'"]*)([.]html)(#[^"\']*|)[\'"]')
big_page_name_re = re.compile ('''(.+?)-big-page''')

def process_i18n_big_page_links (match, prefix, lang_ext):
    big_page_name = big_page_name_re.match (match.group (1))
    if big_page_name:
        destination_path = os.path.normpath (os.path.join (os.path.dirname (prefix),
                                                           big_page_name.group (0)))
        if not lang_ext in pages_dict[destination_path]:
            return match.group (0)
    return 'href="' + match.group (1) + '.' + lang_ext \
        + match.group (2) + match.group (3) + '"'

def process_links (s, prefix, lang_ext, file_name, missing, target):
    page_flavors = {}
    if target == 'online':
        # Strip .html, suffix for auto language selection (content
        # negotiation).  The menu must keep the full extension, so do
        # this before adding the menu.
        page_flavors[file_name] = \
            [lang_ext, online_links_re.sub ('\\1="\\2\\4"', s)]
    elif target == 'offline':
        # in LANG doc index: don't rewrite .html suffixes
        # as not all .LANG.html pages exist;
        # the doc index should be translated and contain links with the right suffixes
        if prefix == 'Documentation/out-www/index':
            page_flavors[file_name] = [lang_ext, s]
        elif lang_ext == '':
            page_flavors[file_name] = [lang_ext, s]
            for e in missing:
                page_flavors[langdefs.lang_file_name (prefix, e, '.html')] = \
                    [e, offline_links_re.sub ('href="\\1.' + e + '\\2\\3"', s)]
        else:
            # For saving bandwidth and disk space, we don't duplicate big pages
            # in English, so we must process translated big pages links differently.
            if 'big-page' in prefix:
                page_flavors[file_name] = \
                    [lang_ext,
                     offline_links_re.sub \
                         (lambda match: process_i18n_big_page_links (match, prefix, lang_ext),
                          s)]
            else:
                page_flavors[file_name] = \
                    [lang_ext,
                     offline_links_re.sub ('href="\\1.' + lang_ext + '\\2\\3"', s)]
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
        page_flavors[k][1] = add_footer (page_flavors[k][1], languages)
    return page_flavors


def process_html_files (package_name = '',
                        package_version = '',
                        target = 'offline',
                        name_filter = lambda s: s):
    """Add header, footer and tweak links to a number of HTML files

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
        subst[e]['footer_report_links'] = subst[e]['footer_report_links'] % subst[e]

    for prefix, ext_list in pages_dict.items ():
        for lang_ext in ext_list:
            file_name = langdefs.lang_file_name (prefix, lang_ext, '.html')
            in_f = open (file_name)
            s = in_f.read()
            in_f.close()

            s = s.replace ('%', '%%')
            s = hack_urls (s, prefix)
            s = add_header (s, prefix)

            ### add footer
            if footer_tag_re.search (s) == None:
                s = add_footer (s, footer_tag + footer)

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
