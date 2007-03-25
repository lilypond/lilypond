#!@PYTHON@

"""
Documentation i18n module
"""

def _ (s):
    return s

def lang_file_name (p, langext, ext):
    if langext != '':
        return p + '.' + langext + ext
    return p + ext

class LanguageDef:
    def __init__ (self, code, name, webext=None, double_punct_char_sep=''):
        self.code = code
        self.name = name
        self.enabled = True
        if webext == None:
            self.webext = self.code
        else:
            self.webext = webext
        self.double_punct_char_sep = double_punct_char_sep
    
    def file_name (self, prefix, ext):
        return lang_file_name (prefix, self.webext, ext)


# All language information needed for documentation i18n is defined
# here. For each 'Documentation/ab' directory containing docs
# translated in 'ab', there should be an entry in LANGUAGES.

site = LanguageDef ('en', _('English'), webext='')
fr = LanguageDef ('fr', _('French'), double_punct_char_sep='&nbsp;')
es = LanguageDef ('es', _('Spanish') )
de = LanguageDef ('de', _('Deutsch') )
#nl = LanguageDef ('nl', 'Nederlands')

# Outdated or broken translations may be disabled
# (please run 'make web-clean' before doing that):
#fr.enabled = False

LANGUAGES = (site, fr, es, de)

if __name__ == '__main__':
    print ' '.join ([l.code for l in LANGUAGES if l.enabled and l.code != 'en'])
else:
    LANGDICT = {}
    for l in LANGUAGES:
        LANGDICT[l.code] = l
