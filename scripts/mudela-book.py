#!@PYTHON@
# All non-english comments are NOT in swedish, they are norwegian!
#  TODO:
# * center option (??)
# * mudela-book should treat \begin{verbatim} that contains inline mudela
#   correctly.
# * make mudela-book understand usepackage{geometry}
# * check that linewidth set in \paper is not wider than actual linewidth?
# * the following fails because mudelabook doesn't care that the
#   last } after \end{mudela} finishes the marginpar:
#     \marginpar{
#     \begin{mudela}
#        c d e f g
#     \end{mudela}}
# * force-verbatim is maybe not that useful since latex fails with footnotes,
#   marginpars and others
# log:
# 0.3:
#   rewrite in Python.
# 0.4:
#   much rewritten by new author. I think the work has been split more
#   logical between different classes.
# 0.4.1:
#   - cleanup
#   - speedup, do all mudela parsing at the same time to avoid multiple
#     guile startup that takes some seconds on my computer
# 0.4.2:
#   - fixed default latex fontsize, it should be 10pt not 11pt
#   - verbatim option no longer visible
#   - mudela-book produces .dvi output
#   - change to use castingalgorithm = \Gourlay instead of \Wordwrap. It gives
#     better result on small linewidths.
#   - mudela-book-doc.doc rewritten
# 0.5:
#   - junked floating and fragment options, added eps option
#   - mudela-book will scan the mudela code to find out if it has to add
#     paper-definition and \score{\notes{...}}
#   - possible to define commands that look like this: \mudela{ c d e }

import os
import string
import re
import getopt
import sys

outdir = 'out'
initfile = ''
program_version = '0.5'

out_files = []

fontsize_i2a = {11:'eleven', 13:'thirteen', 16:'sixteen',
                20:'twenty', 26:'twentysix'}
fontsize_pt2i = {'11pt':11, '13pt':13, '16pt':16, '20pt':20, '26pt':26}

begin_mudela_re = re.compile ('^ *\\\\begin{mudela}')
extract_papersize_re = re.compile('\\\\documentclass[\[, ]*(\w*)paper[\w ,]*\]\{\w*\}')
extract_fontsize_re = re.compile('[ ,\[]*([0-9]*)pt')
begin_mudela_opts_re = re.compile('\[[^\]]*\]')
end_mudela_re = re.compile ('^ *\\\\end{mudela}')
section_re = re.compile ('\\\\section')
chapter_re = re.compile ('\\\\chapter')
input_re = re.compile ('^\\\\input{([^}]*)')
include_re = re.compile ('^\\\\include{([^}]*)')
begin_document_re = re.compile ('^ *\\\\begin{document}')
documentclass_re = re.compile('\\\\documentclass')
twocolumn_re = re.compile('\\\\twocolumn')
onecolumn_re = re.compile('\\\\onecolumn')
preMudelaExample_re = re.compile('\\\\def\\\\preMudelaExample')
postMudelaExample_re = re.compile('\\\\def\\\\postMudelaExample')
boundingBox_re = re.compile('%%BoundingBox: ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*)')

def file_exist_b(name):
    try: 
	f = open(name)
    except IOError:
	return 0
    f.close ()
    return 1

def ps_dimention(fname):
    fd = open(fname)
    lines = fd.readlines()
    for line in lines:
        s = boundingBox_re.search(line)
        if s:
            break
    return (int(s.groups()[2])-int(s.groups()[0]), 
            int(s.groups()[3])-int(s.groups()[1]))


class CompileStatus:
    pass
class SomethingIsSeriouslyBroken:
    pass

def file_mtime (name):
    return os.stat (name)[8] #mod time

def need_recompile_b(infile, outfile):
    indate = file_mtime (infile)
    try:
        outdate = file_mtime (outfile)
        return indate > outdate
    except os.error:
        return 1

#
# executes os.system(command) if infile is newer than
# outfile or outfile don't exist
#
def compile (command, workingdir, infile, outfile):
    "Test if infile is newer than outfile. If so, cd to workingdir"
    "and execute command"
    indate = file_mtime (workingdir+infile)
    try:
        outdate = file_mtime (workingdir+outfile)
        recompile = indate > outdate

    except os.error:
        recompile = 1

    if recompile:
        sys.stderr.write ('invoking `%s\'\n' % command)
        if workingdir == '':
            status = os.system (command)
        else:
            status = os.system ('cd %s; %s' %(workingdir, command))
        if status:
            raise CompileStatus

class Properties:
    #
    # init
    #
    def __init__(self):
        self.__linewidth = {
            1: {'a4':{10: 345, 11: 360, 12: 390},
                'a5':{10: 276, 11: 276, 12: 276},
                'b5':{10: 345, 11: 356, 12: 356},
                'letter':{10: 345, 11: 360, 12: 390},
                'legal': {10: 345, 11: 360, 12: 390},
                'executive':{10: 345, 11: 360, 12: 379}},
            2: {'a4':{10: 167, 11: 175, 12: 190},
                'a5':{10: 133, 11: 133, 12: 133},
                'b5':{10: 167, 11: 173, 12: 173},
                'letter':{10: 167, 11: 175, 12: 190},
                'legal':{10: 167, 11: 175, 12: 190},
                'executive':{10: 167, 11: 175, 12: 184}}}
        # >0 --> force all mudela to this pt size
        self.force_mudela_fontsize = 0
        self.force_verbatim_b = 0
        self.__data = {
            'mudela-fontsize' : {'init': 16},
            'papersize' : {'init': 'a4'},
            'num-column' : {'init': 1},
            'tex-fontsize' : {'init': 10}
            }
    def clear_for_new_file(self):
        for var in self.__data.keys():
            self.__data[var] = {'init': self.__data[var]['init']}
    def clear_for_new_block(self):
        for var in self.__data.keys():
            if self.__data[var].has_key('block'):
                del self.__data[var]['block']
    def __get_variable(self, var):
        if self.__data[var].has_key('block'):
            return self.__data[var]['block']
        elif self.__data[var].has_key('file'):
            return self.__data[var]['file']
        else:
            return self.__data[var]['init']
    def setPapersize(self, size, requester):
        self.__data['papersize'][requester] = size
    def getPapersize(self):
        return self.__get_variable('papersize')
    def setMudelaFontsize(self, size, requester):
        self.__data['mudela-fontsize'][requester] = size
    def getMudelaFontsize(self):
        if self.force_mudela_fontsize:
            return self.force_mudela_fontsize
        return self.__get_variable('mudela-fontsize')
    def setTexFontsize(self, size, requester):
        self.__data['tex-fontsize'][requester] = size
    def getTexFontsize(self):
        return self.__get_variable('tex-fontsize')
    def setNumColumn(self, num, requester):
        self.__data['num-column'][requester] = num
    def getNumColumn(self):
        return self.__get_variable('num-column')
    def getLineWidth(self):
        return self.__linewidth[self.getNumColumn()][self.getPapersize()][self.getTexFontsize()]


class Mudela_output:
    def __init__ (self, basename):
        self.basename = basename
        self.temp_filename = "%s/%s" %(outdir, 'mudela-temp.ly')
        self.file = open (self.temp_filename, 'w')
        self.__lines = []
        # 'tex' or 'eps'
        self.graphic_type = 'tex'
        self.code_type = 'unknown'
    def write (self, line):
        # match only if there is nothing but whitespace before \begin HACK
        if re.search('^\s*\\\\begin{mudela}', line):
            self.scan_begin_statement(line)
        else:
            if self.code_type == 'unknown':
                if re.search('^\s*\\\\score', line) or \
                   re.search('^\s*\\\\paper', line) or \
                   re.search('^\s*\\\\header', line) or \
                   re.search('^\s*[A-Za-z]*\s*=', line):
                    self.code_type = 'ly'
            self.__lines.append(line)
    def scan_begin_statement(self, line):
        r  = begin_mudela_opts_re.search(line)
        if r:
            o = r.group()[1:-1]
            optlist =  re.compile('[\s,]*').split(o)
        else:
            optlist = []
        if 'fragment' in optlist:
            print "warning: obsolete option: fragment"
        if 'floating' in optlist:
            print "warning: obsolete option: floating, change to eps"
        if 'eps' in optlist:
            self.graphic_type = 'eps'
        for pt in fontsize_pt2i.keys():
            if pt in optlist:
                Props.setMudelaFontsize(fontsize_pt2i[pt], 'block')
    def write_red_tape(self):
        self.file.write ('\\include \"paper%d.ly\"\n' \
                         % Props.getMudelaFontsize())
                         
        s = fontsize_i2a[Props.getMudelaFontsize()]
        if self.code_type == 'fly':
            linewidth_str = 'linewidth = -1.\cm;'
        else:
            linewidth_str = 'linewidth = %i.\\pt;' % Props.getLineWidth()
        self.file.write("\\paper {"
                        + "\\paper_%s " % s
                        + linewidth_str
                        + "castingalgorithm = \Gourlay; \n}")
                        #+ "castingalgorithm = \Wordwrap; indent = 2.\cm; \n}")
        if self.code_type == 'fly':
            self.file.write('\\score{\n\\notes{')
    def close (self):
        if self.code_type == 'unknown':
            self.code_type = 'fly'
        if self.code_type == 'ly':
            self.write_red_tape()
            for l in self.__lines:
                self.file.write(l)
        else:
            self.write_red_tape()
            for l in self.__lines:
                self.file.write(l)
            self.file.write('}}')
            
        self.file.close()

        inf = outdir + self.basename + '.ly'
        outf = outdir + self.basename + '.tex'
        if not os.path.isfile(inf):
            status = 1
        else:
            status = os.system ('diff -q %s %s' % (self.temp_filename, inf))
        if status:
            os.rename (self.temp_filename, inf)
        if need_recompile_b(inf, outf):
            out_files.append((self.graphic_type, inf))
    def insert_me_string(self):
        "Returns a string that can be used directly in latex."
        if self.graphic_type == 'tex':
            return ['tex', self.basename]
        elif self.graphic_type == 'eps':
            return ['eps', self.basename]
        else:
            raise SomethingIsSeriouslyBroken

class Tex_output:
    def __init__ (self, name):
        self.output_fn = '%s/%s' % (outdir, name)
        self.__lines = []
    def open_verbatim (self, line):
        self.__lines.append('\\begin{verbatim}\n')
        s = re.sub('[\s,]*verbatim[\s]*', '', line)
        s = re.sub('\[\]', '', s)
        self.__lines.append(s);
    def close_verbatim (self):
        self.__lines.append('\\end{verbatim}\n')
    def write (self, s):
        self.__lines.append(s)
    def create_graphics(self):
        s = ''
        g_vec = []
        for line in self.__lines:
            if type(line)==type([]):
                g_vec.append(line)
        for g in g_vec:
            if need_recompile_b(outdir+g[1]+'.ly', outdir+g[1]+'.tex'):
                    s = s + ' ' + g[1]+'.ly'
        if s != '':
            e = os.system('cd %s; lilypond %s' %(outdir, s))
            if e:
                print "error: lilypond exited with value", e
                sys.exit(e)
        for g in g_vec:
            if g[0] == 'eps':
                compile('tex %s' % g[1]+'.tex', outdir, g[1]+'.tex', g[1]+'.dvi')
                compile('dvips -E -o %s %s' %(g[1]+'.eps', g[1]+'.dvi'), outdir,
                        g[1]+'.dvi', g[1]+'.eps')
    def write_outfile(self):
        file = open(self.output_fn+'.latex', 'w')
        file.write('% Created by mudela-book\n')
        for line in self.__lines:
            if type(line)==type([]):
                if line[0] == 'tex':
                    file.write('\\preMudelaExample\\input %s\n\postMudelaExample '\
			       # TeX applies the prefix of the main source automatically.
                               % (line[1]+'.tex'))
#                               % (outdir+line[1]+'.tex'))
                if line[0] == 'eps':
                    ps_dim = ps_dimention(outdir+line[1]+'.eps')
                    file.write('\\parbox{%ipt}{\includegraphics{%s}}\n' \
                               % (ps_dim[0], line[1]+'.eps'))
#                               % (ps_dim[0], outdir+line[1]+'.eps'))
            else:
                file.write(line)
        file.close()
class Tex_input:
    def __init__ (self, filename):
        for fn in [filename, filename+'.tex', filename+'.doc']:
            try:
                self.infile = open (fn)
                self.filename = fn
                return
            except:
                continue
        raise IOError
    def get_lines (self):
        lines = self.infile.readlines ()
        (retlines, retdeps) = ([],[self.filename])
        for line in lines:
            r_inp = input_re.search (line)
            r_inc = include_re.search (line)

            # Filename rules for \input :
            # input: no .tex ext
            # 1. will search for file with exact that name (tex-input.my will be found)
            # 2. will search for file with .tex ext, (tex-input.my
            #    will find tex-input.my.tex)
            # input: with .tex ext
            # 1. will find exact match
            
            # Filename rules for \include :
            # 1. will only find files with name given to \include + .tex ext
            if r_inp:
                try:
                    t = Tex_input (r_inp.groups()[0])
                    ls = t.get_lines ()
                    retlines = retlines + ls[0]
                    retdeps = retdeps + ls[1]
                except:
                    print "warning: can't find %s, let's hope latex will" \
                          % r_inp.groups()[0]
                    retlines.append (line)
            elif r_inc:
                try:
                    t = Tex_input (r_inc.groups()[0]+'.tex')
                    ls =t.get_lines ()
                    ls[0].insert(0, '\\newpage\n')
                    ls[0].append('\\newpage\n')
                    retlines = retlines + ls[0]
                    retdeps = retdeps + ls[1]
                except:
                    print "warning: can't find %s, let's hope latex will" \
                          % r_inc.groups()[0]
                    retlines.append (line)
            else:
                r_mud = defined_mudela_cmd_re.search(line)
                if r_mud:
                    ss = "\\\\verb(?P<xx>[^a-zA-Z])\s*\\\\%s\s*(?P=xx)" \
                         % re.escape(r_mud.group()[1:])
                    if re.search(ss, line):
                        retlines.append(line)
                        continue
                    while 1:
                        opts = r_mud.groups()[2]
                        if opts == None:
                            opts = ''
                        else:
                            opts = ', '+opts
                        (start, rest) = string.split(line, r_mud.group(), 1)
                        retlines.append(start+'\n')
                        v = string.split(defined_mudela_cmd[r_mud.groups()[0]], '\n')
                        for l in v[1:-1]:
                            l = re.sub(r'\\fontoptions', opts, l)
                            l = re.sub(r'\\maininput', r_mud.groups()[3], l)
                            retlines.append(l)
                        r_mud = defined_mudela_cmd_re.search(rest)
                        if not r_mud:
                            retlines.append(rest)
                            break;
                        line = rest
                else:
                    retlines.append (line)
        else:
            return (retlines, retdeps)


class Main_tex_input(Tex_input):
    def __init__ (self, name, outname):

        Tex_input.__init__ (self, name) # ugh
        self.outname = outname
        self.chapter = 0
        self.section = 0
        self.fine_count =0
        self.mudtex = Tex_output (self.outname)
        self.mudela = None
        self.deps = []
        self.verbatim = 0
        # set to 'mudela' when we are processing mudela code,
        # both verbatim and graphic-to-be
        self.mode = 'latex'
    def set_sections (self, l):
        if section_re.search (l):
            self.section = self.section + 1
        if chapter_re.search (l):
            self.section = 0
            self.chapter = self.chapter + 1

    def gen_basename (self):
        return '%s-%d.%d.%d' % (self.outname, self.chapter,
                                self.section, self.fine_count)
    def extract_papersize_from_documentclass(self, line):
        pre = extract_papersize_re.search(line)
        if not pre:
            return None
        return pre.groups()[0]
    def extract_fontsize_from_documentclass(self, line):
        r = extract_fontsize_re.search(line)
        if r:
            return int(r.groups()[0])
    def do_it(self):
        preMudelaDef = postMudelaDef = 0
        (lines, self.deps) = self.get_lines ()
        for line in lines:
            if documentclass_re.search (line):
                p = self.extract_papersize_from_documentclass (line)
                if p:
                    Props.setPapersize(p, 'file')
                f = self.extract_fontsize_from_documentclass (line)
                if f:
                    Props.setTexFontsize (f, 'file')
            elif twocolumn_re.search (line):
                Props.setNumColumn (2, 'file')
            elif onecolumn_re.search (line):
                Props.setNumColumn (1, 'file')
            elif preMudelaExample_re.search (line):
                preMudelaDef = 1
            elif postMudelaExample_re.search (line):
                postMudelaDef = 1
            elif begin_document_re.search (line):
                if not preMudelaDef:
                    self.mudtex.write ('\\def\\preMudelaExample{}\n')
                if not postMudelaDef:
                    self.mudtex.write ('\\def\\postMudelaExample{}\n')
            elif begin_mudela_re.search (line):
                Props.clear_for_new_block()
                if __debug__:
                    if self.mode == 'mudela':
                        raise AssertionError
                self.mode = 'mudela'
                r  = begin_mudela_opts_re.search (line)
                if r:
                    o = r.group()[1:][:-1]
                    optlist =  re.compile('[ ,]*').split(o)
                else:
                    optlist = []
                if ('verbatim' in optlist) or (Props.force_verbatim_b):
                    self.verbatim = 1
                    self.mudtex.open_verbatim (line)
                    continue
                else:
                    self.verbatim = 0
                    self.mudela = Mudela_output (self.gen_basename ())
            elif end_mudela_re.search (line):
                if __debug__:
                    if self.mode != 'mudela':
                        raise AssertionError
                if self.mudela:
                    self.mudela.close ()
                    self.mudtex.write (self.mudela.insert_me_string())
                    del self.mudela
                    self.mudela = None
                    self.fine_count = self.fine_count + 1
                else:                    
                    self.mudtex.write (line)
                    self.mudtex.close_verbatim ()
                self.mode = 'latex'
                continue

            if self.mode == 'mudela' and not self.verbatim:
                self.mudela.write (line)
            else:
                self.mudtex.write (line)
                self.set_sections(line)
        self.mudtex.create_graphics()
        self.mudtex.write_outfile()
        del self.mudtex
		

def help():
    sys.stdout.write("""Usage: mudela-book [options] FILE\n
Generate hybrid LaTeX input from Latex + mudela
Options:\n
  -h, --help                     print this help
  -d, --outdir=DIR               directory to put generated files
  -o, --outname=FILE             prefix for filenames
  --default-mudela-fontsize=??pt default fontsize for music
  --force-mudela-fontsize=??pt   force fontsize for all inline mudela
  --force-verbatim               make all mudela verbatim\n
  --dependencies                 write dependencies
  --init                         mudela-book initfile
  """
		     )
    sys.exit (0)


def write_deps (fn, out,  deps):
	out_fn = outdir + '/' + fn
	print '`writing `%s\'\n\'' % out_fn
	
	f = open (out_fn, 'w')
	f.write ('%s: %s\n'% (outdir + '/' + out + '.dvi',
			      reduce (lambda x,y: x + ' '+ y, deps)))
	f.close ()

def identify():
    sys.stderr.write ('This is %s version %s\n' % ('mudela-book', program_version))

def main():
    global outdir, initfile, defined_mudela_cmd, defined_mudela_cmd_re
    outname = ''
    try:
        (options, files) = getopt.getopt(
            sys.argv[1:], 'hd:o:', ['outdir=', 'outname=',
                                    'default-mudela-fontsize=',
                                    'force-mudela-fontsize=',
                                    'help', 'dependencies',
                                    'force-verbatim', 'init='])
    except getopt.error, msg:
        print "error:", msg
        sys.exit(1)
        
    do_deps = 0
    for opt in options:    
	o = opt[0]
	a = opt[1]
	if o == '--outname' or o == '-o':
            if len(files) > 1:
                #HACK
                print "Mudela-book is confused by --outname on multiple files"
                sys.exit(1)
            outname = a
        if o == '--outdir' or o == '-d':
            outdir = a
        if o == '--help' or o == '-h':
            help ()
	if o == '--dependencies':
            do_deps = 1
        if o == '--default-mudela-fontsize':
            if not fontsize_pt2i.has_key(a):
                print "Error: illegal fontsize:", a
                print " accepted fontsizes are: 11pt, 13pt, 16pt, 20pt, 26pt"
                sys.exit()
            Props.setMudelaFontsize(fontsize_pt2i[a], 'init')
	if o == '--force-mudela-fontsize':
            if not fontsize_pt2i.has_key(a):
                print "Error: illegal fontsize:", a
                print " accepted fontsizes are: 11pt, 13pt, 16pt, 20pt, 26pt"
                sys.exit()
            Props.force_mudela_fontsize = fontsize_pt2i[a]
        if o == '--force-verbatim':
            Props.force_verbatim_b = 1
        if o == '--init':
            initfile =  a
    if outdir[-1:] != '/':
        outdir = outdir + '/'

    std_init_filename = ''
    for p in string.split(os.environ['LILYINCLUDE'], ':'):
        try:
            std_init_filename =  p+os.sep+'mudela-book-defs.py'
            break
        except:
            continue
    defined_mudela_cmd_re = {}
    try:
	f = open(std_init_filename)
	s = f.read()
	f.close()
	defined_mudela_cmd = eval(s)	# UGH
    except IOError, w:
        sys.stderr.write("%s (`%s')\n" % (w[1], std_init_filename))
#        sys.exit(1)


    if initfile != '':
        f = open(initfile)
        s = f.read()
        f.close()
        d = eval(s)
        for i in d.keys():
            defined_mudela_cmd[i] = d[i]
        del d

    c = defined_mudela_cmd.keys()[0]
    for x in defined_mudela_cmd.keys()[1:]:
        c = c + '|'+x
    defined_mudela_cmd_re = re.compile("\\\\(%s)(\[(\d*pt)\])*{([^}]*)}" %c)

    if not os.path.isdir(outdir):
        os.system('mkdir %s' % outdir)

    for input_filename in files:
        Props.clear_for_new_file()
        if outname:
            my_outname = outname
        else:
            my_outname = os.path.basename(os.path.splitext(input_filename)[0])
        my_depname = my_outname + '.dep'        
        inp = Main_tex_input (input_filename, my_outname)
        inp.do_it ()
#        os.system('latex %s/%s.latex' % (outdir, my_outname))
        if do_deps:
            write_deps (my_depname, my_outname, inp.deps)

identify()
Props = Properties()
main()
