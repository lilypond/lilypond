#!/usr/bin/env python

# fixcc -- indent and space lily's c++ code

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2005--2020 Jan Nieuwenhuizen <janneke@gnu.org>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


#  Performs string substitution on files, then applies astyle
#  (http://astyle.sourceforge.net)
# TODO
#  Remove prefiltering as the equivalent formatting becomes available in
#  astyle, or as the prefiltering is deemed un-necessary.
#  Soon, this script might be replaced by a simple invocation of astyle

import __main__
import getopt
import os
import re
import sys
import time
import subprocess

COMMENT = 'COMMENT'
STRING = 'STRING'
GLOBAL_CXX = 'GC++'
CXX = 'C++'
verbose_p = 0
indent_p = 1
PREFERRED_ASTYLE_VERSION = "Artistic Style Version 3.1"


rules = {
    GLOBAL_CXX:
    [
        # delete trailing whitespace
        ('[ \t]*\n', '\n'),
    ],
    CXX:
    [
        # space before parenthesis open; astyle -xd does this except for foo().
        (r'([\w\)\]])\(', '\\1 ('),
        # delete inline double spaces
        (r'(\S)  +', '\\1 '),
        # delete space before parenthesis close
        (r' *\)', ')'),
        # delete spaces after prefix
        (r'(--|\+\+) *([\w\(])', '\\1\\2'),
        # delete spaces before postfix
        (r'([\w\)\]]) *(--|\+\+)', '\\1\\2'),

        # delete space around operator
        (r'([\w\(\)\]]) *(\.|->) *([\w\(\)])', '\\1\\2\\3'),
        # delete space after operator
        (r'(::) *([\w\(\)])', '\\1\\2'),

        # delete superflous space around operator
        (r'([\w\(\)\]]) +(&&|\|\||<=|>=|!=|\|=|==|\+=|-=|\*=|/=|\?|<|>|\+|-|=|/|:|&|\||\*) +([\w\(\)])', '\\1 \\2 \\3'),

        # trailing operator, but don't un-trail close angle-braces > nor pointer *, and not before a preprocessor line
        (r'(?<!\s) (::|&&|\|\||<=|>=|!=|\|=|==|\+=|-=|\*=|/=|\?|<|\+|-|=|/|:|&XXX|\||\*XXX) *\n( *)([^\s#])', '\n\\2\\1 \\3'),
        # space after `operator'
        (r'(\Woperator) *([^\w\s])', '\\1 \\2'),
        # trailing parenthesis open
        (r'\( *\n *', '('),
        # dangling parenthesis close: Disabled to leave ADD_TRANSLATOR format in place
        #('\n *\)', ')'),
        # dangling comma
        ('\n( *),', ',\n\\1'),
        # delete space after case, label
        (r'(\W(case|label) [\w]+) :', '\\1:'),
        # delete space before comma
        (' +,', ','),
        # delete space before semicolon
        ('([^;]) +;', '\\1;'),
        # dangling newline
        ('\n\n+', '\n\n'),

        # delete backslash before empty line (emacs' indent region is broken)
        ('\\\\\n\n', '\n\n'),
    ],

    COMMENT:
    [
        # delete empty first lines
        (r'(/\*\n)\n*', '\\1'),
        # delete empty last lines
        (r'\n*(\n\*/)', '\\1'),
        # delete newline after start?
        #('/(\*)\n', '\\1'),
        # delete newline before end?
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
        'define':
        r'''(?x)
    (?P<match>
    (?P<code>
    \#[ \t]*define[ \t]+([^\n]*\\\n)*[^\n]*))''',

        'multiline_comment':
        r'''(?sx)
    (?P<match>
    (?P<code>
    [ \t]*/\*.*?\*/))''',

        'singleline_comment':
        r'''(?mx)
    ^.*?    # leave leading spaces for the comment snippet
    (?P<match>
    (?P<code>
    [ \t]*//[^\n]*\n))''',

        'string':
        r'''(?x)
    "      # leave the leading " character visible to CXX rules
    (?P<match>
    (?P<code>
    ([^"\n]|\\")*"))''',

        'char':
        r'''(?x)
    (?P<match>
    (?P<code>
    '([^']+|\')))''',

        'include':
        r'''(?x)
    (?P<match>
    (?P<code>
    \#[ \t]*include[ \t]*<[^>]*>))''',
    },
}


class Chunk:
    def replacement_text(self):
        return ''

    def filter_text(self):
        return self.replacement_text()


class Substring (Chunk):
    def __init__(self, source, start, end):
        self.source = source
        self.start = start
        self.end = end

    def replacement_text(self):
        s = self.source[self.start:self.end]
        if verbose_p:
            sys.stderr.write('CXX Rules')
        for i in rules[CXX]:
            if verbose_p:
                sys.stderr.write('.')
                #sys.stderr.write ('\n\n***********\n')
                #sys.stderr.write (i[0])
                #sys.stderr.write ('\n***********\n')
                #sys.stderr.write ('\n=========>>\n')
                #sys.stderr.write (s)
                #sys.stderr.write ('\n<<=========\n')
            s = re.sub(i[0], i[1], s)
        if verbose_p:
            sys.stderr.write('done\n')
        return s


class Snippet (Chunk):
    def __init__(self, type, match, format):
        self.type = type
        self.match = match
        self.hash = 0
        self.options = []
        self.format = format

    def replacement_text(self):
        return self.match.group('match')

    def substring(self, s):
        return self.match.group(s)

    def __repr__(self):
        return repr(self.__class__) + ' type = ' + self.type


class Multiline_comment (Snippet):
    def __init__(self, source, match, format):
        self.type = type
        self.match = match
        self.hash = 0
        self.options = []
        self.format = format

    def replacement_text(self):
        s = self.match.group('match')
        if verbose_p:
            sys.stderr.write('COMMENT Rules')
        for i in rules[COMMENT]:
            if verbose_p:
                sys.stderr.write('.')
            s = re.sub(i[0], i[1], s)
        return s


snippet_type_to_class = {
    'multiline_comment': Multiline_comment,
    #        'string': Multiline_comment,
    #        'include': Include_snippet,
}


def find_toplevel_snippets(s, types):
    if verbose_p:
        sys.stderr.write('Dissecting')

    res = {}
    for i in types:
        res[i] = re.compile(snippet_res[format][i])

    snippets = []
    index = 0
    # found = dict (map (lambda x: (x, None),
    # types))
    # urg python2.1
    found = {}
    list(map(lambda x, f=found: f.setdefault(x, None),
             types))

    # We want to search for multiple regexes, without searching
    # the string multiple times for one regex.
    # Hence, we use earlier results to limit the string portion
    # where we search.
    # Since every part of the string is traversed at most once for
    # every type of snippet, this is linear.

    while True:
        if verbose_p:
            sys.stderr.write('.')
        first = None
        endex = 1 << 30
        for type in types:
            if not found[type] or found[type][0] < index:
                found[type] = None
                m = res[type].search(s[index:endex])
                if not m:
                    continue

                cl = Snippet
                if type in snippet_type_to_class:
                    cl = snippet_type_to_class[type]
                snip = cl(type, m, format)
                start = index + m.start('match')
                found[type] = (start, snip)

            if found[type] \
                and (not first
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
            snippets.append(Substring(s, index, len(s)))
            break

        (start, snip) = found[first]
        snippets.append(Substring(s, index, start))
        snippets.append(snip)
        found[first] = None
        index = start + len(snip.match.group('match'))

    return snippets


def nitpick_file(outdir, file):
    s = open(file, encoding='utf8').read()

    t = s.expandtabs(8)
    for i in rules[GLOBAL_CXX]:
        t = re.sub(i[0], i[1], t)

    # FIXME: Containing blocks must be first, see
    #        find_toplevel_snippets.
    #        We leave simple strings be part of the code
    snippet_types = (
        'define',
        'multiline_comment',
        'singleline_comment',
        'string',
        #                'char',
        'include',
    )

    chunks = find_toplevel_snippets(t, snippet_types)
    # code = filter (lambda x: is_derived_class (x.__class__, Substring),
    #               chunks)

    t = ''.join([x.filter_text() for x in chunks])
    fixt = file
    if s != t:
        if not outdir:
            os.system('mv %s %s~' % (file, file))
        else:
            fixt = os.path.join(outdir,
                                os.path.basename(file))
        h = open(fixt, "w", encoding="utf8")
        h.write(t)
        h.close()
    if s != t or indent_p:
        indent_file(fixt)


def indent_file(file):
  # Astyle aborts on unrecognized options,
  # so wait until everyone has 2.04 before considering:
  # --attach-namespaces --indent-namespaces \
  # --max-code-length=80 --pad-first-paren-out \
    astyle = '''astyle\
  --options=none --quiet -n \
  --style=gnu --indent=spaces=2 \
  --max-instatement-indent=60 \
  --indent-cases \
  --align-pointer=name --pad-oper \
  --keep-one-line-blocks \
  %(file)s
  ''' % vars()
    if verbose_p:
        sys.stderr.write(astyle)
        sys.stderr.write('\n')
    os.system(astyle)


def usage():
    sys.stdout.write(r'''
Usage:
fixcc [OPTION]... FILE...

Options:
 --help
 --lazy   skip astyle, if no changes
 --sloppy accept any astyle version
 --verbose
 --test

Typical use with LilyPond:

 scripts/auxiliar/fixcc.py $(git ls-files '*.cc' '*.hh')

''')


def do_options():
    global indent_p, outdir, verbose_p, PREFERRED_ASTYLE_VERSION
    (options, files) = getopt.getopt(sys.argv[1:], '',
                                     ['help', 'lazy', 'outdir=', 'sloppy',
                                      'test', 'verbose'])
    for (o, a) in options:
        if o == '--help':
            usage()
            sys.exit(0)
        elif o == '--lazy':
            indent_p = 0
        elif o == '--outdir':
            outdir = a
        elif o == '--sloppy':
            PREFERRED_ASTYLE_VERSION = "Artistic Style"
        elif o == '--verbose':
            verbose_p = 1
        elif o == '--test':
            test()
            sys.exit(0)
        else:
            assert unimplemented
    if not files:
        usage()
        sys.exit(2)
    return files


def check_astyle_version():
    cmd = "astyle --version"
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    return (PREFERRED_ASTYLE_VERSION in stderr.decode()) \
        or (PREFERRED_ASTYLE_VERSION in stdout.decode())


outdir = 0
format = CXX
socketdir = '/tmp/fixcc'
socketname = 'fixcc%d' % os.getpid()


def main():
    files = do_options()
    if not check_astyle_version():
        print("Warning: try to use %s." % PREFERRED_ASTYLE_VERSION)
        print("Please limit use of this version to files with changed code.")
        if len(files) > 4:
            print("Too many files with this version.  See `astyle --help`")
            sys.exit(1)
    if outdir and not os.path.isdir(outdir):
        os.makedirs(outdir)
    for i in files:
        sys.stderr.write('%s...\n' % i)
        nitpick_file(outdir, i)


# TODO: make this compilable and check with g++
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
 Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
 1 && * Moment::unsmob (lf);
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


def test():
    test_file = 'fixcc.cc'
    open(test_file, 'w', encoding='utf8').write(TEST)
    nitpick_file(outdir, test_file)
    sys.stdout.write(open(test_file, encoding='utf8').read())


if __name__ == '__main__':
    main()
