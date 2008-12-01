#!/usr/bin/python

# fixcc -- nitpick lily's c++ code

# TODO
#  * maintainable rules: regexp's using whitespace (?x) and match names
#    <identifier>)
#  * trailing `*' vs. function definition
#  * do not break/change indentation of fixcc-clean files
#  * check lexer, parser
#  * rewrite in elisp, add to cc-mode
#  * using regexes is broken by design
#  * ?
#  * profit

import __main__
import getopt
import os
import re
import string
import sys
import time

COMMENT = 'COMMENT'
STRING = 'STRING'
GLOBAL_CXX = 'GC++'
CXX = 'C++'
verbose_p = 0
indent_p = 0

rules = {
    GLOBAL_CXX:
    [
    # delete gratuitous block
    ('''\n(    |\t)\s*{\n\s*(.*?)(?![{}]|\b(do|for|else|if|switch|while)\b);\n\s*}''',
    '\n\\2;'),
    ],
    CXX:
    [
    # space before parenthesis open
    ('([^\( \]])[ \t]*\(', '\\1 ('),
    # space after comma
    ("\([^'],\)[ \t]*", '\1 '),
    # delete gratuitous block
    ('''\n(    |\t)\s*{\n\s*(.*?)(?![{}]|\b(do|for|else|if|switch|while)\b);\n\s*}''',
    '\n\\2;'),
    # delete inline tabs
    ('(\w)\t+', '\\1 '),
    # delete inline double spaces
    ('   *', ' '),
    # delete space after parenthesis open
    ('\([ \t]*', '('),
    # delete space before parenthesis close
    ('[ \t]*\)', ')'),
    # delete spaces after prefix
    ('(--|\+\+)[ \t]*([\w\)])', '\\1\\2'),
    # delete spaces before postfix
    ('([\w\)\]])[ \t]*(--|\+\+)', '\\1\\2'),
    # delete space after parenthesis close
    #('\)[ \t]*([^\w])', ')\\1'),
    # delete space around operator
    # ('([\w\(\)\]])([ \t]*)(::|\.)([ \t]*)([\w\(\)])', '\\1\\3\\5'),
    ('([\w\(\)\]])([ \t]*)(\.|->)([ \t]*)([\w\(\)])', '\\1\\3\\5'),
    # delete space after operator
    ('(::)([ \t]*)([\w\(\)])', '\\1\\3'),
    # delete superflous space around operator
    ('([\w\(\)\]])([ \t]+)(&&|\|\||<=|>=|!=|\|=|==|\+=|-=|\*=|/=|\?|<|>|\+|-|=|/|:|&|\||\*)([ \t]+)([\w\(\)])', '\\1 \\3 \\5'),
    # space around operator1
    ('([\w\)\]]) *(&&|\|\||<=|>=|!=|\|=|==|\+=|-=|\*=|/=|\?|<|>|=|/|:|&|\||\*) *([\w\(])', '\\1 \\2 \\3'),
    # space around operator2
    ('([\w\)\]]) *(&&|\|\||<=|>=|!=|\|=|==|\+=|-=|\*=|/=|\?|<|>|=|/|:|&|\||\*) ([^\w\s])', '\\1 \\2 \\3'),
    # space around operator3
    ('([^\w\s]) (&&|\|\||<=|>=|!=|\|=|==|\+=|-=|\*=|/=|\?|<|[^-]>|=|/|:|&|\||\*) *([\w\(])', '\\1 \\2 \\3'),
    # space around operator4
    ('([\w\(\)\]]) (\*|/|\+|-) *([-:])', '\\1 \\2 \\3'),
    # space around +/-; exponent
    ('([\w\)\]])(\+|-)([_A-Za-z\(])', '\\1 \\2 \\3'),
    ('([_\dA-Za-df-z\)\]])(\+|-)([\w\(])', '\\1 \\2 \\3'),
    # trailing operator
    (' (::|&&|\|\||<=|>=|!=|\|=|==|\+=|-=|\*=|/=|\?|<|>|\+|-|=|/|:|&XXX|\||\*XXX)[ \t]*\n([ \t]*)',         '\n\\2\\1 '),
    # pointer
    ##('(bool|char|const|delete|int|stream|unsigned|void|size_t|struct \w+|[A-Z]\w*|,|;|&&|<|[^-]>|\|\||-|\+)[ \t]*(\*|&)[ \t]*', '\\1 \\2'),
    ('(bool|char|const|delete|int|stream|unsigned|void|size_t|struct \w+|[A-Z]\w*|,|;|:|=|\?\)|&&|<|[^-]>|\|\||-|\+)[ \t]*(\*|&)[ \t]*', '\\1 \\2'),
    #to#('(bool|char|const|delete|int|stream|unsigned|void|([A-Z]\w*)|[,])[ \n\t]*(\*|&)[ \t]*', '\\1 \\3'),
    # pointer with template
    ('(( *((bool|char|const|delete|int|stream|unsigned|void|size_t|class[ \t]+\w*|[A-Z]\w*|\w+::\w+|[,])[ \*&],*)+)>) *(\*|&) *', '\\1 \\5'),
    #to#('(( *((bool|char|delete|int|stream|unsigned|void|(class[ \t]+\w*)|([A-Z]\w*)|[,])[ \*&],*)+)>)[ \t\n]*(\*|&) *', '\\1 \\7'),
    # unary pointer, minus, not
    ('(return|=) (\*|&|-|!) ([\w\(])', '\\1 \\2\\3'),
    # space after `operator'
    ('(\Woperator) *([^\w\s])', '\\1 \\2'),
    # dangling brace close
    ('\n[ \t]*(\n[ \t]*})', '\\1'),
    # dangling newline
    ('\n[ \t]*\n[ \t]*\n', '\n\n'),
    # dangling parenthesis open
    #('[ \t]*\n[ \t]*\([ \t]*\n', '('),
    ('\([ \t]*\n', '('),
    # dangling parenthesis close
    ('\n[ \t]*\)', ')'),
    # dangling comma
    ('\n[ \t]*,', ','),
    # dangling semicolon
    ('\n[ \t]*;', ';'),
    # brace open
    ('(\w)[ \t]*([^\s]*){([ \t]*\n)', '\\1\\2\n{\n'),
    # brace open backslash
    ('(\w[^\n]*){[ \t]*\\\\\n', '\\1\\\n{\\\n'),
    # brace close
    ("}[ \t]*([^'\n]*\w[^\n\\\]*)\n", '}\n\\1\n'),
    # brace close backslash
    ("}[ \t]*([^'\n]*\w[^\n\\\]*)", '\n}\n\\1'),
    # delete space after `operator'
    #('(\Woperator) (\W)', '\\1\\2'),
    # delete space after case, label
    ('(\W(case|label) ([\w]+)) :', '\\1:'),
    # delete space before comma
    ('[ \t]*,', ','),
    # delete space before semicolon
    ('[ \t]*;', ';'),
    # delete space before eol-backslash
    ('[ \t]*\\\\\n', '\\\n'),
    # delete trailing whitespace
    ('[ \t]*\n', '\n'),

    ## Deuglify code that also gets ugly by rules above.
    # delete newline after typedef struct
    ('(typedef struct\s+([\w]*\s){([^}]|{[^}]*})*})\s*\n\s*(\w[\w\d]*;)', '\\1 \\4'),
    # delete spaces around template brackets
    #('(dynamic_cast|template|([A-Z]\w*))[ \t]*<[ \t]*(( *(bool|char|int|unsigned|void|(class[ \t]+\w*)|([A-Z]\w*)),?)+)[ \t]?(| [\*&])[ \t]*>', '\\1<\\3\\8>'),
    ('(dynamic_cast|template|typedef|\w+::\w+|[A-Z]\w*)[ \t]*<[ \t]*(( *(bool|char|const|int|unsigned|void|size_t|class[ \t]+\w*|[A-Z]\w*)( *[\*&]?,|[\*&])*)+)[ \t]?(| [\*&])[ \t]*>', '\\1<\\2\\6>'),
    ('(\w+::\w+|[A-Z]\w*) < ((\w+::\w+|[A-Z]\w*)<[A-Z]\w*>) >', '\\1<\\2 >'),
    ('((if|while)\s+\(([^\)]|\([^\)]*\))*\))\s*;', '\\1\n;'),
    ('(for\s+\(([^;]*;[^;]*;([^\)]|\([^\)]*\))*)\))\s*;', '\\1\n;'),
    # do {..} while
    ('(}\s*while\s*)(\(([^\)]|\([^\)]*\))*\))\s*;', '\\1\\2;'),

    ## Fix code that gets broken by rules above.
    ##('->\s+\*', '->*'),
    # delete space before #define x()
    ('#[ \t]*define (\w*)[ \t]*\(', '#define \\1('),
    # add space in #define x ()
    ('#[ \t]*define (\w*)(\(([^\(\)]|\([^\(\)]*\))*\)\\n)',
    '#define \\1 \\2'),
    # delete space in #include <>
    ('#[ \t]*include[ \t]*<[ \t]*([^ \t>]*)[ \t]*(/?)[ \t]*([^ \t>]*)[ \t]*>',
    '#include <\\1\\2\\3>'),
    # delete backslash before empty line (emacs' indent region is broken)
    ('\\\\\n\n', '\n\n'),
    ],

    COMMENT:
    [
    # delete trailing whitespace
    ('[ \t]*\n', '\n'),
    # delete empty first lines
    ('(/\*\n)\n*', '\\1'),
    # delete empty last lines
    ('\n*(\n\*/)', '\\1'),
    ## delete newline after start?
    #('/(\*)\n', '\\1'),
    ## delete newline before end?
    #('\n(\*/)', '\\1'),
    ],
    }

# Recognize special sequences in the input.
#
#   (?P<name>regex) -- Assign result of REGEX to NAME.
#   *? -- Match non-greedily.
#   (?m) -- Multiline regex: Make ^ and $ match at each line.
#   (?s) -- Make the dot match all characters including newline.
#   (?x) -- Ignore whitespace in patterns.
no_match = 'a\ba'
snippet_res = {
    CXX: {
    'multiline_comment':
    r'''(?sx)
    (?P<match>
    (?P<code>
    [ \t]*/\*.*?\*/))''',
    
    'singleline_comment':
    r'''(?mx)
    ^.*
    (?P<match>
    (?P<code>
    [ \t]*//([ \t][^\n]*|)\n))''',

    'string':
    r'''(?x)
    (?P<match>
    (?P<code>
    "([^\"\n](\")*)*"))''',
    
    'char':
    r'''(?x)
    (?P<match>
    (?P<code>
    '([^']+|\')))''',
     
     'include':
     r'''(?x)
     (?P<match>
     (?P<code>
     "#[ \t]*include[ \t]*<[^>]*>''',
     },
     }

class Chunk:
    def replacement_text (self):
        return ''

    def filter_text (self):
        return self.replacement_text ()

class Substring (Chunk):
    def __init__ (self, source, start, end):
        self.source = source
        self.start = start
        self.end = end

    def replacement_text (self):
        s = self.source[self.start:self.end]
        if verbose_p:
            sys.stderr.write ('CXX Rules')
        for i in rules[CXX]:
            if verbose_p:
                sys.stderr.write ('.')
                #sys.stderr.write ('\n\n***********\n')
                #sys.stderr.write (i[0])
                #sys.stderr.write ('\n***********\n')
                #sys.stderr.write ('\n=========>>\n')
                #sys.stderr.write (s)
                #sys.stderr.write ('\n<<=========\n')
            s = re.sub (i[0], i[1], s)
        if verbose_p:
            sys.stderr.write ('done\n')
        return s
        

class Snippet (Chunk):
    def __init__ (self, type, match, format):
        self.type = type
        self.match = match
        self.hash = 0
        self.options = []
        self.format = format

    def replacement_text (self):
        return self.match.group ('match')

    def substring (self, s):
        return self.match.group (s)

    def __repr__ (self):
        return `self.__class__` + ' type = ' + self.type

class Multiline_comment (Snippet):
    def __init__ (self, source, match, format):
        self.type = type
        self.match = match
        self.hash = 0
        self.options = []
        self.format = format

    def replacement_text (self):
        s = self.match.group ('match')
        if verbose_p:
            sys.stderr.write ('COMMENT Rules')
        for i in rules[COMMENT]:
            if verbose_p:
                sys.stderr.write ('.')
            s = re.sub (i[0], i[1], s)
        return s

snippet_type_to_class = {
    'multiline_comment': Multiline_comment,
#        'string': Multiline_comment,
#        'include': Include_snippet,
}

def find_toplevel_snippets (s, types):
    if verbose_p:
        sys.stderr.write ('Dissecting')

    res = {}
    for i in types:
        res[i] = re.compile (snippet_res[format][i])

    snippets = []
    index = 0
    ## found = dict (map (lambda x: (x, None),
    ##                      types))
    ## urg python2.1
    found = {}
    map (lambda x, f = found: f.setdefault (x, None),
      types)

    # We want to search for multiple regexes, without searching
    # the string multiple times for one regex.
    # Hence, we use earlier results to limit the string portion
    # where we search.
    # Since every part of the string is traversed at most once for
    # every type of snippet, this is linear.

    while 1:
        if verbose_p:
            sys.stderr.write ('.')
        first = None
        endex = 1 << 30
        for type in types:
            if not found[type] or found[type][0] < index:
                found[type] = None
                m = res[type].search (s[index:endex])
                if not m:
                    continue

                cl = Snippet
                if snippet_type_to_class.has_key (type):
                    cl = snippet_type_to_class[type]
                snip = cl (type, m, format)
                start = index + m.start ('match')
                found[type] = (start, snip)

            if found[type] \
             and (not first \
                or found[type][0] < found[first][0]):
                first = type

                # FIXME.

                # Limiting the search space is a cute
                # idea, but this *requires* to search
                # for possible containing blocks
                # first, at least as long as we do not
                # search for the start of blocks, but
                # always/directly for the entire
                # @block ... @end block.

                endex = found[first][0]

        if not first:
            snippets.append (Substring (s, index, len (s)))
            break

        (start, snip) = found[first]
        snippets.append (Substring (s, index, start))
        snippets.append (snip)
        found[first] = None
        index = start + len (snip.match.group ('match'))

    return snippets

def nitpick_file (outdir, file):
    s = open (file).read ()

    for i in rules[GLOBAL_CXX]:
        s = re.sub (i[0], i[1], s)

    # FIXME: Containing blocks must be first, see
    #        find_toplevel_snippets.
    #        We leave simple strings be part of the code
    snippet_types = (
        'multiline_comment',
        'singleline_comment',
        'string',
#                'char',
        )

    chunks = find_toplevel_snippets (s, snippet_types)
    #code = filter (lambda x: is_derived_class (x.__class__, Substring),
    #               chunks)

    t = string.join (map (lambda x: x.filter_text (), chunks), '')
    fixt = file
    if s != t:
        if not outdir:
            os.system ('mv %s %s~' % (file, file))
        else: 
            fixt = os.path.join (outdir,
                      os.path.basename (file))
        h = open (fixt, "w")
        h.write (t)
        h.close ()
    if s != t or indent_p:
        indent_file (fixt)

def indent_file (file):
    emacs = '''emacs\
    --no-window-system\
    --batch\
    --no-site-file\
    --no-init-file\
    %(file)s\
    --eval '(let ((error nil)
           (version-control nil))
        (load-library "cc-mode")
        (c++-mode)
        (indent-region (point-min) (point-max))
        (if (buffer-modified-p (current-buffer))
         (save-buffer)))' ''' % vars ()
    emacsclient = '''emacsclient\
    --socket-name=%(socketdir)s/%(socketname)s\
    --no-wait\
    --eval '(let ((error nil)
           (version-control nil))
        (load-library "cc-mode")
        (find-file "%(file)s")
        (c++-mode)
        (indent-region (point-min) (point-max))
        (if (buffer-modified-p (current-buffer))
         (save-buffer)))' ''' \
         % { 'file': file,
           'socketdir' : socketdir,
           'socketname' : socketname, }
    if verbose_p:
        sys.stderr.write (emacs)
        sys.stderr.write ('\n')
    os.system (emacs)


def usage ():
    sys.stdout.write (r'''
Usage:
fixcc [OPTION]... FILE...

Options:
 --help
 --indent   reindent, even if no changes
 --verbose
 --test

Typical use with LilyPond:

 fixcc $(find flower kpath-guile lily -name '*cc' -o -name '*hh' | grep -v /out)

This script is licensed under the GNU GPL
''')

def do_options ():
    global indent_p, outdir, verbose_p
    (options, files) = getopt.getopt (sys.argv[1:], '',
                     ['help', 'indent', 'outdir=',
                     'test', 'verbose'])
    for (o, a) in options:
        if o == '--help':
            usage ()
            sys.exit (0)
        elif o == '--indent':
            indent_p = 1
        elif o == '--outdir':
            outdir = a
        elif o == '--verbose':
            verbose_p = 1
        elif o == '--test':
            test ()
            sys.exit (0)
        else:
            assert unimplemented
    if not files:
        usage ()
        sys.exit (2)
    return files


outdir = 0
format = CXX
socketdir = '/tmp/fixcc'
socketname = 'fixcc%d' % os.getpid ()

def setup_client ():
    #--no-window-system\
    #--batch\
    os.unlink (os.path.join (socketdir, socketname))
    os.mkdir (socketdir, 0700)
    emacs='''emacs\
        --no-site-file\
        --no-init-file\
        --eval '(let ((error nil)
               (version-control nil))
            (load-library "server")
            (setq server-socket-dir "%(socketdir)s")
            (setq server-name "%(socketname)s")
            (server-start)
            (while t) (sleep 1000))' ''' \
            % { 'socketdir' : socketdir,
              'socketname' : socketname, }
              
    if not os.fork ():
        os.system (emacs)
        sys.exit (0)
    while not os.path.exists (os.path.join (socketdir, socketname)):
        time.sleep (1)

def main ():
    #emacsclient should be faster, but this does not work yet
    #setup_client ()
    files = do_options ()
    if outdir and not os.path.isdir (outdir):
        os.makedirs (outdir)
    for i in files:
        sys.stderr.write ('%s...\n' % i)
        nitpick_file (outdir, i)


## TODO: make this compilable and check with g++
TEST = '''
#include <libio.h>
#include <map>
class
ostream ;

class Foo {
public: static char* foo ();
std::map<char*,int>* bar (char, char) { return 0; }
};
typedef struct
{
 Foo **bar;
} String;

ostream &
operator << (ostream & os, String d);

typedef struct _t_ligature
{
 char *succ, *lig;
 struct _t_ligature * next;
}  AFM_Ligature;
 
typedef std::map < AFM_Ligature const *, int > Bar;

 /**
 (c) 1997--2008 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */
 
/*      ||
*      vv
* !OK  OK
*/
/*     ||
   vv
 !OK  OK
*/
char *
Foo:: foo ()
{
int
i
;
 char* a= &++ i ;
 a [*++ a] = (char*) foe (*i, &bar) *
 2;
 int operator double ();
 std::map<char*,int> y =*bar(-*a ,*b);
 Interval_t<T> & operator*= (T r);
 Foo<T>*c;
 int compare (Pqueue_ent < K, T > const& e1, Pqueue_ent < K,T> *e2);
 delete *p;
 if (abs (f)*2 > abs (d) *FUDGE)
  ;
 while (0);
 for (; i<x foo(); foo>bar);
 for (; *p && > y;
   foo > bar)
;
 do {
 ;;;
 }
 while (foe);

 squiggle. extent;
 1 && * unsmob_moment (lf);
 line_spanner_ = make_spanner ("DynamicLineSpanner", rq ? rq->*self_scm
(): SCM_EOL);
 case foo: k;

 if (0) {a=b;} else {
 c=d;
 }

 cookie_io_functions_t Memory_out_stream::functions_ = {
  Memory_out_stream::reader,
  ...
 };

 int compare (Array < Pitch> *, Array < Pitch> *);
 original_ = (Grob *) & s;
 Drul_array< Link_array<Grob> > o;
}

 header_.char_info_pos = (6 + header_length) * 4;
 return ly_bool2scm (*ma < * mb);

 1 *::sign(2);

 (shift) *-d;

 a = 0 ? *x : *y;

a = "foo() 2,2,4";
{
 if (!span_)
  {
   span_ = make_spanner ("StaffSymbol", SCM_EOL);
  }
}
{
 if (!span_)
  {
   span_ = make_spanner (StaffSymbol, SCM_EOL);
  }
}
'''

def test ():
    test_file = 'fixcc.cc'
    open (test_file, 'w').write (TEST)
    nitpick_file (outdir, test_file)
    sys.stdout.write (open (test_file).read ())

if __name__ == '__main__':
    main ()

