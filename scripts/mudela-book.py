#!@PYTHON@
# All non-english comments are NOT in swedish, they are norwegian!

#  TODO:  center option (??)
# * One empty, not two line after a mudela{floating} should be enough
#   to get a new paragraph.
# * clean up handling of filename of inputfile
# * the   verbatim  option should not be visible in the created latex file
# * what the h.. does castingalgorithm do/mean???
# * the following fails because mudelabook doesn't care that the
#   last } after \end{mudela} finishes the marginpar:
#     \marginpar{
#     \begin{mudela}[fragment]
#        c d e f g
#     \end{mudela}}
# log:

# 0.3:
#   rewrite in Python.
# 0.4:
#   much rewritten by new author. I think the work has been split more
#   logical between different classes.
#   

import os
import string
import re
import getopt
import sys
import regsub

outdir = 'out'
program_version = '0.4'
default_paper_size_global = 'a4'
default_mudela_fontsize = '16pt'
force_mudela_fontsize_b = 0

EXPERIMENTAL = 0
out_files = []

fontsize_i2a = {11:'eleven', 13:'thirteen', 16:'sixteen', 20:'twenty', 26:'twentysix'}
fontsize_pt2i = {'11pt':11, '13pt':13, '16pt':16, '20pt':20, '26pt':26}

begin_mudela_re = re.compile ('^ *\\\\begin{mudela}')
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


class PaperDef:
    __onecolumn_linewidth = {
        'a4':{'10pt': 345, '11pt': 360, '12pt':390},
        'a5':{'10pt': 276, '11pt': 276, '12pt':276},
        'b5':{'10pt': 345, '11pt': 356, '12pt':356},
        'letter':{'10pt': 345, '11pt': 360, '12pt':390},
        'legal':{'10pt': 345, '11pt': 360, '12pt':390},
        'executive':{'10pt': 345, '11pt': 360, '12pt':379}
        }
    __twocolumn_linewidth = {
         'a4':{'10pt': 167, '11pt': 175, '12pt':190},
        'a5':{'10pt': 133, '11pt': 133, '12pt':133},
        'b5':{'10pt': 167, '11pt': 173, '12pt':173},
        'letter':{'10pt': 167, '11pt': 175, '12pt':190},
        'legal':{'10pt': 167, '11pt': 175, '12pt':190},
        'executive':{'10pt': 167, '11pt': 175, '12pt':184}
        }
    __numcolumn = 1
    __fontsize = '11pt'
    #
    # init
    #
    def __init__(self):
        self.__papersize = default_paper_size_global
    def set_papersize (self, p):
        if not self.__onecolumn_linewidth.has_key(p):
            print "warning:unsupported papersize", p, \
                  "will use", default_paper_size_global
            self.__papersize = default_paper_size_global
        else:
            self.__papersize = p
    def set_latex_fontsize(self, pt):
        self.__fontsize = pt
    def get_linewidth (self):
        if self.__numcolumn == 1:
            return self.__onecolumn_linewidth[self.__papersize][self.__fontsize]
        else:
            return self.__twocolumn_linewidth[self.__papersize][self.__fontsize]
    def onecolumn (self):
        self.__numcolumn = 1
    def twocolumn (self):
        self.__numcolumn = 2


class Mudela_output:
    def __init__ (self, basename):
        self.basename = basename
        # it's an integer!
        self.feta_pt_size = fontsize_pt2i[default_mudela_fontsize]
        self.temp_filename = "%s/%s" %(outdir, 'mudela-temp.ly')
        self.file = open (self.temp_filename, 'w')
        # 'tex' or 'eps'
        self.graphic_type = 'tex'
        self.fragment = 0
    def write (self, line):
        # match only if there is nothing but whitespace before \begin
        if re.search('^\s*\\\\begin{mudela}', line):
            self.scan_begin_statement(line)
            self.write_red_tape()
        else:
            self.file.write (line)
    def scan_begin_statement(self, line):
        global force_mudela_fontsize_b;
        r  = begin_mudela_opts_re.search(line)
        if r:
            o = r.group()[1:][:-1]
            optlist =  re.compile('[ ,]*').split(o)
        else:
            optlist = []
        if 'floating' in optlist:
            self.graphic_type = 'eps'
        else:
            self.graphic_type = 'tex'
        if 'fragment' in optlist:
            self.fragment = 1
        else:
            self.fragment = 0
        for pt in fontsize_pt2i.keys():
            if pt in optlist:
                if force_mudela_fontsize_b:
                    self.feta_pt_size = fontsize_pt2i[default_mudela_fontsize]
                else:
                    self.feta_pt_size = fontsize_pt2i[pt]
    def write_red_tape(self):
        self.file.write ('\\include \"paper%d.ly\"\n' % self.feta_pt_size)
        s = fontsize_i2a[self.feta_pt_size]
        if self.fragment:
            self.file.write("default_paper = \\paper {"
                            + "\\paper_%s\n linewidth = -1.\\pt;" % s
                            + "castingalgorithm = \Wordwrap; indent = 2.\cm; \n}")
            self.file.write("\\score{\n\\notes{") #HACK
        else:
            self.file.write ("default_paper = \\paper {"
                             + "\\paper_%s\n linewidth = %i.\\pt;" % \
                             (s, Paper.get_linewidth()) \
                             + "castingalgorithm = \Wordwrap; indent = 2.\cm;\n}")
    def close (self):
        if self.fragment:
            self.file.write ('}\\paper { \\default_paper; } }\n')
        self.file.close ()

        inf = self.basename + '.ly'
        outf = self.basename + '.tex'
        if not file_exist_b (inf):
            status = 1
        else:
            status = os.system ('diff -q %s %s' % (self.temp_filename, inf))
        if status:
            os.rename (self.temp_filename, inf)
        if EXPERIMENTAL:
            if need_recompile_b(inf, outf):
                print "Need recompile"
                out_files.append((self.graphic_type, inf))
        else:
            compile ('lilypond  -o %s %s;'%  (self.basename, inf), '', inf, outf)
            if self.graphic_type == 'eps':
                bname = self.basename[string.rfind(self.basename, '/')+1:]
                tex_name = bname+'.tex'
                dvi_name = bname+'.dvi'
                eps_name = bname+'.eps'
                compile ('tex %s' % tex_name, outdir, tex_name, dvi_name)
                compile ('dvips -E -o %s %s' % (eps_name, dvi_name), outdir, dvi_name, eps_name)
    def insert_me_string(self):
        "Returns a string that can be used directly in latex."
        if self.graphic_type == 'tex':
            return '\\preMudelaExample\\input %s\n\postMudelaExample\n' % self.basename
        elif self.graphic_type == 'eps':
            ps_dim = ps_dimention('%s.eps' % self.basename)
            return '\\parbox{%ipt}{\includegraphics{%s.eps}}' % (ps_dim[0], self.basename)
        else:
            print "Unsupported graphic type '%s'" % self.graphic_type
            sys.exit(1)

class Tex_output:
    def __init__ (self, name):
        self.output_fn = '%s/%s' % (outdir, name)
        self.file = open (self.output_fn , 'w')
    def open_mudela (self, basename):
        self.mudela_basename = basename
    def open_verbatim (self):
        self.file.write ('\\begin{verbatim}\n')
    def close_verbatim (self):
        self.file.write ('\\end{verbatim}\n')
    def write (self, s):
        self.file.write (s)

class Tex_input:
    def __init__ (self,name):
        # HACK
        print "name=", name
        try:
            self.infile = open (name)
        except IOError:
            if (name[-4:] != '.tex') and (name[-4:] != '.doc'):
                try:
                    name = name + '.doc'
                    self.infile = open (name)
                except IOError:
                    name = name + '.tex'
                    self.infile = open(name)
        self.filename = name
            
    def get_lines (self):
        lines = self.infile.readlines ()
        (retlines, retdeps) = ([],[self.filename])
        for line in lines:
            r = input_re.search (line)
            ri = include_re.search (line)
            if r:
                try:
                    t = Tex_input (r.groups()[0])
                    ls =t.get_lines ()
                except IOError:
                    # HACK should not warn about files like lilyponddefs, only
                    # files we think is a part of the document and include
                    # mudela that need parsing
                    print "warning: can't find file " % r.grops()[0]
                    ls = [[line], []]
                retlines = retlines + ls[0]
                retdeps = retdeps + ls[1]
            elif ri:
                try:
                    t = Tex_input (ri.groups()[0])
                    ls =t.get_lines ()
                    ls[0].insert(0, '\\newpage')
                    ls[0].append('\\newpage')
                except IOError:
                    print "warning: can't find include file:",  ri.groups()[0]
                    ls = [[line], []];
                retlines = retlines + ls[0]
                retdeps = retdeps + ls[1]
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
        return '%s/%s-%d.%d.%d' % (outdir, self.outname,self.chapter,self.section,self.fine_count)

    def extract_papersize_from_documentclass(self, line):
        pre = re.search('\\\\documentclass[\[, ]*(\w*)paper[\w ,]*\]\{\w*\}', line)
        if not pre:
            return default_paper_size_global
        return pre.groups()[0]
    def extract_fontsize_from_documentclass(self, line):
        if re.search('\\\\documentclass\[[^\]]*\]\{[^\}]*\}', line):
            r = re.search('[ ,\[]*([0-9]*pt)', line)
            if r:
                return r.groups()[0]
        return '10pt'
    def do_it(self):
        preMudelaDef = postMudelaDef = 0
        (lines, self.deps) = self.get_lines ()
        for line in lines:
            if documentclass_re.search (line):
                Paper.set_papersize (self.extract_papersize_from_documentclass (line) )
                Paper.set_latex_fontsize (self.extract_fontsize_from_documentclass (line) )
            elif twocolumn_re.search (line):
                Paper.twocolumn ()
            elif onecolumn_re.search (line):
                Paper.onecolumn ()
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
                if 'verbatim' in optlist:
                    self.verbatim = 1
                    self.mudtex.open_verbatim ()
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
        del self.mudtex
		

def help():
    sys.stdout.write("Usage: mudela-book [options] FILE\n"
		 + "Generate hybrid LaTeX input from Latex + mudela"
		 + "Options:\n"
		 + "  -h, --help             print this help\n"
		 + "  -d, --outdir=DIR       directory to put generated files\n" 
		 + "  -o, --outname=FILE     prefix for filenames\n"
                 + "  --mudela-fontsize=??pt default fontsize when no parameter for \\begin{mudela}\n"
                 + "  --force-mudela-fontsize=??pt force fontsize for all inline mudela\n"
		     )
    sys.exit (0)


def write_deps (fn, out,  deps):
	out_fn = outdir + '/' + fn
	print '\`writing \`%s\'\n\'' % out_fn
	
	f = open (out_fn, 'w')
	f.write ('%s: %s\n'% (outdir + '/' + out + '.dvi',
			      reduce (lambda x,y: x + ' '+ y, deps)))
	f.close ()

def identify():
    sys.stderr.write('*** Lokal versjon av mudela-book ***\n')
    sys.stderr.write ('This is %s version %s\n' % ('mudela-book', program_version))

def main():
    global default_mudela_fontsize, force_mudela_fontsize_b, outdir
    outname = ''
    try:
        (options, files) = getopt.getopt(
            sys.argv[1:], 'hd:o:', ['outdir=', 'outname=', 'mudela-fontsize=',
                                    'force-mudela-fontsize=', 'help', 'dependencies'])
    except getopt.error, msg:
        print "error:", msg
        sys.exit(1)
        
    do_deps = 0
    for opt in options:
	o = opt[0]
	a = opt[1]
	if o == '--outname' or o == '-o':
            outname = a
        if o == '--outdir' or o == '-d':
            outdir = a
        if o == '--help' or o == '-h':
            help ()
	if o == '--dependencies':
            do_deps = 1
        if o == '--mudela-fontsize':
            default_mudela_fontsize = a
	if o == '--force-mudela-fontsize':
            default_mudela_fontsize = a
            force_mudela_fontsize_b = 1

    if outdir[-1:] != '/':
        outdir = outdir + '/'

    if not file_exist_b(outdir):
        os.system('mkdir %s' % outdir)

    if not fontsize_pt2i.has_key(default_mudela_fontsize):
        print "warning: fontsize %s is not supported using 16pt" % default_mudela_fontsize
        default_mudela_fontsize = '16pt'
    
    for f in files:
        my_outname = outname
        if not my_outname:
            my_outname = regsub.sub ('\\(.*\\)\\.doc', '\\1', f)

        my_depname = my_outname + '.dep'
        
        inp = Main_tex_input (f, my_outname)
        inp.do_it ()

    if do_deps:
		write_deps (my_depname, my_outname, inp.deps)



identify()
Paper = PaperDef()
main()
if EXPERIMENTAL:
    print "outfile:", out_files
    for i in out_files:
        print "skal gjøre", i
