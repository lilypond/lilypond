#!@PYTHON@
#
# The bugs you find are made by Tom Cato Amundsen <tomcato@xoommail.com>
# Send patches/questions/bugreports to a mailinglist:
#  gnu-music-discuss@gnu.org
#  bug-gnu-music@gnu.org
#  help-gnu-music@gnu.org
#
#  TODO:
# * center option (??)
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
#   - don't produce .dvi output, it was a silly idea...
# 0.5.1:
#   - removed init/mudela-book-defs.py, \mudela is defined inside mudela-book
#   - fragment and nonfragment options will override autodetection of type of
#     in mudela-block (voice contents or complete code)
# 0.5.2:
#   - fixed verbatim option behaviour: don't show \begin{mudela} \end{mudela}
#     and show both mudela code and music
#   - veryverbatim option, same as verbatim but show \begin{mudela}
#     and \end{mudela}. (saves keystrokes in mudela-book-doc.doc)
#   - intertext="bla bla bla" option
#   - mudela-book now understand latex \begin{verbatim}
# 0.5.3:
#   - bf: \mudela{ \times 2/3{...} }
#        * \t in \times is not tab character and
#        * dont treat the first '}' as command ending
# 0.5.4: (Mats B)
#   - .fly and .sly files in \mudelafile{} are treated as standalone Lilypond.
#   - Fragments, .fly and .sly files are in \relative mode.
# 0.5.5: (Mats B)
#   - bf: Default fragments have linewidth=-1.0
#   - Added 'singleline' and 'multiline' options.
# 0.5.6:
#   - \mudelafile{} set linewidth correct, -1 for .sly and texlinewidth for .fly
#   - changes to Mudela_output
#   - changed RE to search for pre/postMudelaExample to make it possible to
#     comment out a definition.
#   - use sys.stderr and sys.stdout instead of print
# 1.1.62
#   - junked separate versioning
#   - pass -I options to lily.
#   - portability stuff (os.sep).

import os
import string
import re
import getopt
import sys
import __main__

outdir = 'out'
initfile = ''
program_version = '@TOPLEVEL_VERSION@'
include_path = ['.']

out_files = []

fontsize_i2a = {11:'eleven', 13:'thirteen', 16:'sixteen',
                20:'twenty', 26:'twentysix'}
fontsize_pt2i = {'11pt':11, '13pt':13, '16pt':16, '20pt':20, '26pt':26}

# perhaps we can do without this?

begin_mudela_re = re.compile ('^ *\\\\begin{mudela}')
begin_verbatim_re = re.compile ('^ *\\\\begin{verbatim}')
end_verbatim_re = re.compile ('^ *\\\\end{verbatim}')
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
mudela_file_re = re.compile('\\\\mudelafile{([^}]+)}')
file_ext_re = re.compile('.+\\.([^.}]+$)')
preMudelaExample_re = re.compile('^\s*\\\\def\\\\preMudelaExample')
postMudelaExample_re = re.compile('^\s*\\\\def\\\\postMudelaExample')
boundingBox_re = re.compile('%%BoundingBox: ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)')
intertext_re = re.compile("intertext=\"([^\"]*)\"")

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


def find_file (name):
    for a in include_path:
	try:
	    nm = os.path.join (a, name)
	    f = open (nm)
	    return nm
	except IOError:
	    pass
    return ''


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
    print "COMPILE!!"
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
    """ Using only self.code_type to deside both value of linewith and
    if we have to put code into \score{...} was silly. Now we use:
    self.code_type:  show us what need to be added.
                        None : init value
                        'NOTES' : add \context Voice{ ... }
                        'CONTEXT' : add \score{ ... }
                        'COMPLETE' : jupp
    self.single_line_b:   0 : linewidth=-1  1: linewith=textwidth
    """
    def __init__ (self, basename):
        self.basename = basename
        self.temp_filename = "%s/%s" %(outdir, 'mudela-temp.ly')
        self.file = open (self.temp_filename, 'w')
        self.__lines = []
        # 'tex' or 'eps'
        self.graphic_type = 'tex'
        self.code_type = None
        self.single_line_b = 1
        self.optlist = []
    def write (self, line):
        # match only if there is nothing but whitespace before \begin.
        # we should not have to do this RE for every line
        if re.search('^\s*\\\\begin{mudela}', line):
            r  = begin_mudela_opts_re.search(line)
            if r:
                o = r.group()[1:-1]
                self.optlist =  re.compile('[\s,]*').split(o)
            else:
                self.optlist = []
        else: # ugh this is NOT bulletproof...
            if not self.code_type:
                if re.search('^\s*%', line) or re.search('^\s*$', line):
                    pass
                elif re.search('^\s*\\\\context', line):
                    self.code_type = 'CONTEXT'
                    self.single_line_b = 0
                elif re.search('^\s*\\\\score', line) or \
                     re.search('^\s*\\\\paper', line) or \
                     re.search('^\s*\\\\header', line) or \
                     re.search('^\s*\\\\version', line) or \
                     re.search('^\s*\\\\include', line) or \
                     re.search('^\s*[A-Za-z]*\s*=', line):
                    self.code_type = 'COMPLETE'
                    self.single_line_b = 0
                else:
                    self.code_type = 'NOTES'
                    self.single_line_b = 1
            self.__lines.append(line)
    def write_red_tape(self):
        if 'eps' in self.optlist:
            self.graphic_type = 'eps'
            #self.single_line_b = 1
        if 'fragment' in self.optlist:
            self.code_type = 'NOTES'
        if 'nonfragment' in self.optlist:
            self.code_type = 'COMPLETE'
        if 'multiline' in self.optlist:
            self.single_line_b = 0
        for pt in fontsize_pt2i.keys():
            if pt in self.optlist:
                Props.setMudelaFontsize(fontsize_pt2i[pt], 'block')
        self.file.write ('\\include \"paper%d.ly\"\n' \
                         % Props.getMudelaFontsize())
                         
        s = fontsize_i2a[Props.getMudelaFontsize()]
        if 'singleline' in self.optlist:
            self.single_line_b = 1
        if self.single_line_b:
            linewidth_str = 'linewidth = -1.\cm;'
        else:
            linewidth_str = 'linewidth = %i.\\pt;' % Props.getLineWidth()
        self.file.write("\\paper {"
                        + "\\paper_%s " % s
                        + linewidth_str
                        + "castingalgorithm = \Gourlay; \n}")
                        #+ "castingalgorithm = \Wordwrap; indent = 2.\cm; \n}")
        if self.code_type == 'CONTEXT':
            self.file.write('\\score{\n\\notes\\relative c{')
        if self.code_type == 'NOTES' :
            self.file.write('\\score{\n\\notes\\relative c{\\context Voice{')
    def close (self):
        self.write_red_tape()
        for l in self.__lines:
            self.file.write(l)
        if self.code_type == 'CONTEXT':
            self.file.write('}}')
        elif self.code_type == 'NOTES':
            self.file.write('}}}')
        self.file.close()

        inf = outdir + self.basename + '.ly'
        outf = outdir + self.basename + '.tex'
        if not os.path.isfile(inf):
            status = 1
        else:
            status = os.system ('diff -q %s %s' % (self.temp_filename, inf))
        if status:
            os.rename (self.temp_filename, inf)

	recompile_b =  need_recompile_b(inf, outf)
	if recompile_b:
            out_files.append((self.graphic_type, inf))
	return recompile_b

    def insert_me_string(self):
        "ugh the name of this function is wrong"
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
    def open_verbatim (self, line, level):
        self.__lines.append('\\begin{verbatim}\n')
        if level == 2:
            s = re.sub('veryverbatim[\s,]*', '', line)
            s = re.sub('intertext=\"([^\"]*)\"[\s,]*', '', s)
            s = re.sub(',\s*\]', ']', s)
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

        lilyoptions = ''
	for inc in __main__.include_path :
		p = inc[:]
		if p[0] <> os.sep and outdir:		# UGH-> win32 ?
			p = '..' + os.sep + p 
		lilyoptions = lilyoptions + " -I \"%s\"" % p
		
        if s != '':
	    cmd = 'cd %s; lilypond %s %s' %(outdir, lilyoptions, s)
	    sys.stderr.write ('invoking command %s'  % cmd)
            e = os.system(cmd)
            if e:
                sys.stderr.write("error: lilypond exited with value %i\n" % e)
                sys.exit(e)
        for g in g_vec:
            if g[0] == 'eps':
                compile('tex %s' % g[1]+'.tex', outdir, g[1]+'.tex', g[1]+'.dvi')
                compile('dvips -E -o %s %s' %(g[1]+'.eps', g[1]+'.dvi'), outdir,
                        g[1]+'.dvi', g[1]+'.eps')
    def write_outfile(self):
        file = open(self.output_fn+'.latex', 'w')
        file.write('% Created by mudela-book\n')
        last_line = None
        for line in self.__lines:
            if type(line)==type([]):
                if last_line == '\n':
                    file.write(r'\vspace{0.5cm}')
                if line[0] == 'tex':                    
                    file.write('\\preMudelaExample \\input %s \\postMudelaExample\n'\
                               % (line[1]+'.tex'))
                if line[0] == 'eps':
                    ps_dim = ps_dimention(outdir+line[1]+'.eps')
                    file.write('\\noindent\\parbox{%ipt}{\includegraphics{%s}}\n' \
                               % (ps_dim[0], line[1]+'.eps'))
            else:
                file.write(line)
            if type(last_line)==type([]):
                if line=='\n':
                    file.write(r'\vspace{0.5cm}')
            last_line = line
        file.close()

# given parameter s="\mudela[some options]{CODE} some text and commands"
# it returns a tuple:
#    (CODE, integer)
# where the last number is the index of the ending '}'
def extract_command(s):
    start_found_b = 0
    count = 0
    start = 0
    for idx in range(len(s)):
        if s[idx] == '{':
            if not start_found_b:
                start = idx
                start_found_b = 1
            count = count + 1
        if s[idx] == '}':
            count = count - 1
        if (start_found_b == 1) and (count == 0):
            break
    return s[start+1:idx], idx

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
                    sys.stderr.write("warning: can't find %s, let's hope latex will\n" % r_inp.groups()[0])
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
                    sys.stderr.write("warning: can't find %s, let's hope latex will" % r_inc.groups()[0])
                    retlines.append (line)
            else:
                # This code should be rewritten, it looks terrible
                r_mud = defined_mudela_cmd_re.search(line)
                if r_mud:
		    # TODO document this
                    ss = "\\\\verb(?P<xx>[^a-zA-Z])\s*\\\\%s\s*(?P=xx)" \
                         % re.escape(r_mud.group()[1:])
                    # just append the line if the command is inside \verb|..|
                    if re.search(ss, line):
                        retlines.append(line)
                        continue
                    while 1:
                        opts = r_mud.groups()[2]
                        cmd_start_idx = r_mud.span()[0]
                        if cmd_start_idx > 0:
                            retlines.append(line[:cmd_start_idx])
                            
                        cmd_data, rest_idx = extract_command(line[cmd_start_idx:])
                        rest_idx = rest_idx + cmd_start_idx + 1
                        if opts == None:
                            opts = ''
                        else:
                            opts = ', '+opts
                        
                        v = string.split(defined_mudela_cmd[r_mud.groups()[0]], '\n')
                        for l in v[1:-1]:
                            l = string.replace(l, '\\fontoptions', opts)
                            l = string.replace(l, '\\maininput', cmd_data)
                            retlines.append(l)
                        r_mud = defined_mudela_cmd_re.search(line[rest_idx:])
                        if not r_mud:
                            rs = line[rest_idx:]
                            while rs[0] == " ":
                                rs = rs[1:]
                            if rs != "\n":
                                retlines.append(line[rest_idx:])
                            break;
                        line = line[rest_idx:]
                else:
                    retlines.append (line)
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
        #HACK
        latex_verbatim = 0
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
            elif begin_verbatim_re.search (line):
                latex_verbatim = 1
            elif end_verbatim_re.search (line):
                latex_verbatim = 0
            elif begin_document_re.search (line):
                if not preMudelaDef:
                    self.mudtex.write ('\\def\\preMudelaExample{}\n')
                if not postMudelaDef:
                    self.mudtex.write ('\\def\\postMudelaExample{}\n')

            elif mudela_file_re.search(line):
		r = mudela_file_re.search(line)

		self.mudela = Mudela_output(self.gen_basename())
		fn = r.group (1)
		full_path = find_file (fn)
		if not full_path:
		    sys.stderr.write("error: can't find file '%s'\n" % fn)
		    sys.exit (1)

		f = open (full_path, 'r')
		lines =f.readlines ()
		self.mudela.write ('%% This is a copy of file %s\n' % full_path)
		for x in lines:
		    self.mudela.write (x)
		r = file_ext_re.search(fn)
                if r:
                    if r.group(1) == 'fly':
                        self.mudela.optlist.append('multiline')
		stat =self.mudela.close ()
		if stat:
			sys.stdout.write("(File %s needs recompiling)\n" % full_path)
                self.mudtex.write (self.mudela.insert_me_string())
		self.deps.append (full_path)
		del self.mudela
                self.mudela = None
                self.fine_count = self.fine_count + 1
		continue
            elif begin_mudela_re.search (line) and not latex_verbatim:
                Props.clear_for_new_block()
                if __debug__:
                    if self.mode == 'mudela':
                        raise AssertionError
                self.mode = 'mudela'
                r  = begin_mudela_opts_re.search (line)
                if r:
                    o = r.group()[1:][:-1]
                    optlist =  re.compile('[ ,]*').split(o)
                    m = intertext_re.search(r.group())
                    if m:
                        self.intertext = m.groups()[0]
                    else:
                        self.intertext = None
                else:
                    optlist = []
                if ('veryverbatim' in optlist):
                    self.verbatim = 2
                elif ('verbatim' in optlist) or (Props.force_verbatim_b):
                    self.verbatim = 1
                else:
                    self.verbatim = 0
                if self.verbatim:
                    self.mudtex.open_verbatim (line, self.verbatim)
                self.mudela = Mudela_output (self.gen_basename ())
                self.mudela.write (line)
                continue
            elif end_mudela_re.search (line) and not latex_verbatim:
                if __debug__:
                    if self.mode != 'mudela':
                        raise AssertionError

                if self.verbatim:
                    if self.verbatim == 2:
                        self.mudtex.write (line)
                    self.mudtex.close_verbatim ()
                self.mudela.close ()
                if self.verbatim and self.intertext:
                    self.mudtex.write(self.intertext)
                self.mudtex.write (self.mudela.insert_me_string())
                del self.mudela
                self.mudela = None
                self.fine_count = self.fine_count + 1
                self.mode = 'latex'
                continue


            if self.mode == 'mudela':
                self.mudela.write (line)
                if self.verbatim:
                    self.mudtex.write (line)
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
  --include                      include path
  --init                         mudela-book initfile
  """
		     )
    sys.exit (0)


def write_deps (fn, out,  deps):
	out_fn = os.path.join (outdir, fn)
	
	sys.stdout.write('writing `%s\'\n' % out_fn)
	
	f = open (out_fn, 'w')
	target = re.sub (os.sep + os.sep, os.sep, os.path.join (outdir, out + '.latex'))
	f.write ('%s: %s\n'% (target,
			      reduce (lambda x,y: x + ' '+ y, deps)))
	f.close ()

def identify():
    sys.stderr.write ('This is %s version %s\n' % ('mudela-book', program_version))

def main():
    global outdir, initfile, defined_mudela_cmd, defined_mudela_cmd_re
    outname = ''
    try:
        (options, files) = getopt.getopt(
            sys.argv[1:], 'hd:o:I:', ['outdir=', 'outname=',
                                    'default-mudela-fontsize=',
                                    'force-mudela-fontsize=',
                                    'help', 'dependencies', 'include=',
                                    'force-verbatim', 'init='])
    except getopt.error, msg:
        sys.stderr.write("error: %s" % msg)
        sys.exit(1)
        
    do_deps = 0
    for opt in options:    
	o = opt[0]
	a = opt[1]
	if o == '--include' or o == '-I':
	    include_path.append (a)
	elif o == '--outname' or o == '-o':
            if len(files) > 1:
                #HACK
                sys.stderr.write("Mudela-book is confused by --outname on multiple files")
                sys.exit(1)
            outname = a
        elif o == '--outdir' or o == '-d':
            outdir = a
        elif o == '--help' or o == '-h':
            help ()
	elif o == '--dependencies':
            do_deps = 1
        elif o == '--default-mudela-fontsize':
            if not fontsize_pt2i.has_key(a):
                sys.stderr.write("Error: invalid fontsize: %s" % a)
                sys.stderr.write("  accepted fontsizes are: 11pt, 13pt, 16pt, 20pt, 26pt")
                sys.exit()
            Props.setMudelaFontsize(fontsize_pt2i[a], 'init')
	elif o == '--force-mudela-fontsize':
            if not fontsize_pt2i.has_key(a):
                sys.stderr.write("Error: invalid fontsize: %s" % a)
                sys.stderr.write("  accepted fontsizes are: 11pt, 13pt, 16pt, 20pt, 26pt")
                sys.exit()
            Props.force_mudela_fontsize = fontsize_pt2i[a]
        elif o == '--force-verbatim':
            Props.force_verbatim_b = 1
        elif o == '--init':
            initfile =  a
    if outdir[-1:] != os.sep:
        outdir = outdir + os.sep

    # r""" ... """ means: leave escape seqs alone.
    defined_mudela_cmd = {'mudela': r"""
\begin{mudela}[eps, singleline \fontoptions]
  \context Staff <
    \context Voice{
      \maininput
    }
  >
\end{mudela}
"""}
    if initfile != '':
        f = open(initfile)
        s = f.read()
        f.close()
        d = eval(s)
        for i in d.keys():
            defined_mudela_cmd[i] = d[i]
        del d

    c = string.join (defined_mudela_cmd.keys(), '|')

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
        if do_deps:
            write_deps (my_depname, my_outname, inp.deps)

identify()
Props = Properties()
main()
