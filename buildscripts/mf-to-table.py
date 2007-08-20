#!@PYTHON@

# mf-to-table.py -- convert spacing info in MF logs . 
#
# source file of the GNU LilyPond music typesetter
#
# (c) 1997--2007 Han-Wen Nienhuys <hanwen@cs.uu.nl>

import os
import sys
import getopt
import string
import re
import time

def read_log_file (fn):
    str = open (fn).read ()
    str = re.sub ('\n', '', str)
    str = re.sub ('[\t ]+', ' ', str)

    deps = []
    autolines = []
    def include_func (match, d = deps):
        d.append (match.group (1))
        return ''

    def auto_func (match, a = autolines):
        a.append (match.group (1))
        return ''

    str = re.sub ('\\(([/a-z.A-Z_0-9-]+\\.mf)', include_func, str)
    str = re.sub ('@{(.*?)@}', auto_func, str)

    return (autolines, deps)


class Char_metric:
    def __init__ (self):
        pass

font_family = 'feta'

def parse_logfile (fn):
    (autolines, deps) = read_log_file (fn)
    charmetrics = []
    
    global_info = {
        'filename' : os.path.splitext (os.path.basename (fn))[0]
        }
    group = ''

    for l in autolines:
        tags = string.split (l, '@:')
        if tags[0] == 'group':
            group = tags[1]
        elif tags[0] == 'puorg':
            group = ''
        elif tags[0] == 'char':
            name = tags[9]

            if group:
                name = group + '.' + name
            m = {
                'description': tags[1],
                'name': name,
                'code': string.atoi (tags[2]),
                'breapth': string.atof (tags[3]),
                'width': string.atof (tags[4]),
                'depth': string.atof (tags[5]),
                'height': string.atof (tags[6]),
                'wx': string.atof (tags[7]),
                'wy': string.atof (tags[8]),
            }
            charmetrics.append (m)
        elif tags[0] == 'font':
            global font_family
            font_family = (tags[3])
            # To omit 'GNU' (foundry) from font name proper:
            # name = tags[2:]
            #urg
            if 0: # testing
                tags.append ('Regular')

            encoding = re.sub (' ','-', tags[5])
            tags = tags[:-1]
            name = tags[1:]
            global_info['design_size'] = string.atof (tags[4])
            global_info['FontName'] = string.join (name, '-')
            global_info['FullName'] = string.join (name,' ')
            global_info['FamilyName'] = string.join (name[1:-1],
                                '-')
            if 1:
                global_info['Weight'] = tags[4]
            else: # testing
                global_info['Weight'] = tags[-1]

            global_info['FontBBox'] = '0 0 1000 1000'
            global_info['Ascender'] = '0'
            global_info['Descender'] = '0'
            global_info['EncodingScheme'] = encoding

        elif tags[0] == 'parameter':
            global_info[tags[1]] = tags[2];
            
    return (global_info, charmetrics, deps)





def write_character_lisp_table (file, global_info, charmetrics):

    def conv_char_metric (charmetric):
        f = 1.0
        s = """(%s .
((bbox . (%f %f %f %f))
(subfont . "%s")
(subfont-index . %d)
(attachment . (%f . %f))))
""" %(charmetric['name'],
   -charmetric['breapth'] * f,
   -charmetric['depth'] * f,
   charmetric['width'] * f,
   charmetric['height'] * f,
   global_info['filename'],
   charmetric['code'],
   charmetric['wx'],
   charmetric['wy'])

        return s

    for c in charmetrics:
        file.write (conv_char_metric (c))


def write_global_lisp_table (file, global_info):
    str = ''

    keys = ['staffsize', 'stafflinethickness', 'staff_space',
        'linethickness', 'black_notehead_width', 'ledgerlinethickness',
        'design_size', 
        'blot_diameter'
        ]
    for k in keys:
        if global_info.has_key (k):
            str = str + "(%s . %s)\n" % (k,global_info[k])

    file.write (str)

    
def write_ps_encoding (name, file, global_info, charmetrics):
    encs = ['.notdef'] * 256
    for m in charmetrics:
        encs[m['code']] = m['name']

    file.write ('/%s [\n' % name)
    for m in range (0, 256):
        file.write ('  /%s %% %d\n' % (encs[m], m))
    file.write ('] def\n')


def write_deps (file, deps, targets):
    for t in targets:
        t = re.sub ( '^\\./', '', t)
        file.write ('%s '% t)
    file.write (": ")
    for d in deps:
        file.write ('%s ' % d)
    file.write ('\n')


def help ():
    sys.stdout.write(r"""Usage: mf-to-table [OPTIONS] LOGFILEs

Generate feta metrics table from preparated feta log.

Options:
 -d, --dep=FILE         print dependency info to FILE
 -h, --help             print this help
 -l, --ly=FILE          name output table
 -o, --outdir=DIR       prefix for dependency info
 -p, --package=DIR      specify package

 """)
    sys.exit (0)


(options, files) = \
 getopt.getopt (sys.argv[1:],
        'a:d:ho:p:t:',
        ['enc=',  'outdir=', 'dep=', 'lisp=',
         'global-lisp=',
         'debug', 'help', 'package='])

global_lisp_nm = ''
char_lisp_nm = ''
enc_nm = ''
depfile_nm = ''
lyfile_nm = ''
outdir_prefix = '.'

for opt in options:
    o = opt[0]
    a = opt[1]
    if o == '--dep' or o == '-d':
        depfile_nm = a
    elif o == '--outdir' or o == '-o':
        outdir_prefix = a
    elif o == '--lisp': 
        char_lisp_nm = a
    elif o == '--global-lisp': 
        global_lisp_nm = a
    elif o == '--enc':
        enc_nm = a
    elif o== '--help' or o == '-h':
        help()
    elif o == '--debug':
        debug_b = 1
    else:
        print o
        raise getopt.error

base = os.path.splitext (lyfile_nm)[0]

for filenm in files:
    (g, m, deps) = parse_logfile (filenm)

    enc_name = 'FetaEncoding'
    if re.search ('parmesan', filenm):
        enc_name = 'ParmesanEncoding'
    elif re.search ('feta-brace', filenm):
        enc_name = 'FetaBraceEncoding'
    elif re.search ('feta-alphabet', filenm):
        enc_name = 'FetaAlphabetEncoding';

    write_ps_encoding (enc_name, open (enc_nm, 'w'), g, m)
    write_character_lisp_table (open (char_lisp_nm, 'w'), g, m)  
    write_global_lisp_table (open (global_lisp_nm, 'w'), g)  
    if depfile_nm:
        write_deps (open (depfile_nm, 'wb'), deps,
              [base + '.log', base + '.dvi', base + '.pfa',
               depfile_nm,
               base + '.pfb'])
