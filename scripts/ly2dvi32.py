#!@PYTHON@

name = 'ly2dvi'
version = '0.0.1'

import sys
import os
import getopt
import re
import string
import time
import glob

class Input:

    def __init__(this):
       this.__fd = None 

    def open(this,file):
        for i in [''] + Props.get('include')[0:]:
            ifile = os.path.join(i,file)
            for j in ['','.ly','.fly']:
                jfile = ifile+j
                try:
                    this.__fd = open( jfile, 'r' )
                    return
                except:
                    pass
        sys.exit('ExitNotFound', file)

    def close(this):
        this.__fd.close()

    def type(this):
        firstline = this.__fd.readline()
        this.__fd.seek(0)
        if  re.match('% Creator: GNU LilyPond [0-9]+[.0-9]+',firstline ):
            return 'output'
        else:
            return 'source'
#
# Scan file for variable settings
#
    def setVars(this):	
	"""Scan source file for variable settings"""
        varTable = [
            #   regexp              set method
            #   ------              ----------
            ( 'language',        Props.setLanguage ),
            ( 'latexheaders',    Props.setHeader ),
            ( 'orientation',     Props.setOrientation ),
            ( 'paperpapersize',  Props.setPaperZize ),
            ( 'papertextheight', Props.setTextHeight ),
            ( 'paperlinewidth',  Props.setLineWidth ),
            ( 'filename',        Props.setFilename ),
            ]

        titles={}
        line='prime the pump' # ugh
        while line:
            line=this.__fd.readline()
            m=re.match('\\\\def\\\\mudela([\w]+){(.*)}',line)
            if m:
                for var in varTable:
                    if m.group(1) == var[0]:
                        var[1](m.group(2),'file')
                        break
                for var in Props.get('titledefs'):
                    if m.group(1) == var:
                        titles[var]=m.group(2)
                        break
        Props.setTitles(titles,'file')
        this.__fd.seek(0)

class TeXOutput:

    def __init__(this):
       this.__fd = None 
       this.__base = ''
       this.__outfile = ''

    def write(this,str):
        this.__fd.write(str)

    def start(this,file):
        """Start the latex file"""
        now=time.asctime(time.localtime(time.time()))
        linewidth = Props.get('linewidth')
        textheight = Props.get('textheight')

        if Props.get('orientation') == 'landscape':
            pagewidth = Props.get('pageheight')
            pageheight = Props.get('pagewidth')
        else:
            pageheight = Props.get('pageheight')
            pagewidth = Props.get('pagewidth')
                             
        horizontalMarginArg =  ( (pagewidth - linewidth)/2 )   
        verticalMarginArg =  ( (pageheight - textheight)/2  )

        top="""\
%% Creator: %s
%% Automatically generated from  %s, %s

\\documentclass[%s]{article}

%s 
\\usepackage{geometry}
\\usepackage[latin1]{inputenc} 
%%\\usepackage[T1]{fontenc} 
%s 
%%\\addtolength{\\oddsidemargin}{-1cm} 
%%\\addtolength{\\topmargin}{-1cm} 
%%\\setlength{\\textwidth}{%s} 
%%\\setlength{\\textheight}{%s} 
\\geometry{width=%spt, left=%spt, height=%spt, top=%spt} 
\\input lilyponddefs 
\\input titledefs 
%s 
\\begin{document}
""" % ( program_id(), Props.get('filename'), now, Props.get('papersize'),
        Props.get('language'), Props.get('pagenumber'), linewidth, textheight,
        linewidth, horizontalMarginArg, textheight, verticalMarginArg,
        Props.get('header') )
        
        pathcomp = os.path.splitext(file)
        this.__base = pathcomp[0]
        this.__outfile = '%s.%d%s' % (pathcomp[0], os.getpid(), pathcomp[1])
        try:
            this.__fd = open(this.__outfile,"w")
        except:
            sys.exit('ExitNoWrite', this.__outfile)
        this.write(top)
        this.__mudelaDefs('')
        this.write("""\
\\cmrtwenty% ugh
\\makelilytitle
""") 

    def next(this):
        this.write("""\
\\def\\theopus{}%
\\def\\thepiece{}%
\\def\\mudelaopus{}%
\\def\\mudelapiece{}%
""")
        this.__mudelaDefs("\\def")
        this.write("""\
\\def\\theopus{\\mudelaopus}% ugh
\\def\\thepiece{\\mudelapiece}%
\\makelilypiecetitle
""")

    def __mudelaDefs(this,opt):
        titles = Props.get('titles')
        for key in titles.keys():
            this.write('%s\\mudela%s{%s}%%\n' % (opt,key,titles[key]))

    def end(this):
        outfile=this.__base + '.dvi'
        if Props.get('output') != '':
            outfile = os.path.join(Props.get('output'), outfile )
            
        this.write("""\
\\vfill\\hfill{\\LilyIdString}
\\end{document}
""")
        this.__fd.close()
        stat = os.system('latex \'\\nonstopmode \\input %s\'' %
                         (this.__outfile))
        if stat:
            sys.exit('ExitBadLatex')
        if os.path.isfile(outfile):
            os.remove(outfile)
        os.rename(this.__base + '.' + str(os.getpid()) + '.dvi', outfile)

class Properties:

    def __set(this,var,value,requester):
        if this.__overrideTable[requester] < this.__data[var][1]:
            return 0
        else:
            this.__data[var] = [value, this.__overrideTable[requester]]

    def get(this,var):
        if var == 'include' or var == 'lilyOutputFiles':
            return this.__data[var][0][0:]  # return a copy not a ref
        else:
            return this.__data[var][0]

    def get_texfile_path (this, var):
	    path =''
	    cmd =('kpsewhich tex %s' % var)
	    pipe = os.popen (cmd, 'r')
	    path = pipe.readline ()[:-1] # chop off \n
	    if not path:
		    path = os.path.join(this.get('root'), 'texmf', 'tex', 'lilypond', var)

	    fd = open(path, 'r')
            return fd

    def __init__(this):
        this.__overrideTable = {
            'init'        : 0,
            'environment' : 1,
            'rcfile'      : 2,
            'file'        : 3,
            'commandline' : 4,
            'program'     : 5
            }

        this.__roverrideTable = {} # reverse lookup
        for i in this.__overrideTable.items():
            this.__roverrideTable[i[1]]=i[0]
        
        this.__data = {
            'pagewidth'    :  [597, this.__overrideTable['init']],
            'pageheight'   :  [845, this.__overrideTable['init']],
            'papersize'    :  ['a4paper', this.__overrideTable['init']],
            'textheight'   :  [0, this.__overrideTable['init']],
            'linewidth'    :  [0, this.__overrideTable['init']],
            'orientation'  :  ['portrait', this.__overrideTable['init']],
            'language'     :  ['%', this.__overrideTable['init']],
            'include'      :  [[], this.__overrideTable['init']],
            'debug'        :  [0, this.__overrideTable['init']],
            'keeplilypond' :  [0, this.__overrideTable['init']],
            'keeply2dvi'   :  [0, this.__overrideTable['init']],
            'pagenumber'   :  ['%', this.__overrideTable['init']],
            'separate'     :  [0, this.__overrideTable['init']],
            'output'       :  ['', this.__overrideTable['init']],
            'header'       :  ['%', this.__overrideTable['init']],
            'dependencies' :  [0, this.__overrideTable['init']],
            'root'         :  ['', this.__overrideTable['init']],
            'tmp'          :  ['d:\tmp', this.__overrideTable['init']],
            'filename'     :  ['', this.__overrideTable['init']],
            'titledefs'    :  [[], this.__overrideTable['init']],
            'titles'       :  [{}, this.__overrideTable['init']],
            'lilyOutputFiles' :  [[], this.__overrideTable['init']],
            }

        if os.environ.has_key('LILYINCLUDE'):
            tmp=this.get('include')
            for s in string.split(os.environ['LILYINCLUDE'],os.pathsep):
                tmp.append(s)
            this.__set('include', tmp, 'environment')    

        if os.environ.has_key('LILYPOND'):
            this.__set('root',os.environ['LILYPOND'], 'environment')
        else:
            p=os.path.split(sys.argv[0])
            p=os.path.split(p[0])
            this.__set('root',p[0],'init')

	if os.environ.has_key ('TEXINPUTS'):
		t = os.pathsep + os.environ['TEXINPUTS']
        os.environ['TEXINPUTS'] = os.path.join(this.get('root'), 'texmf',
					       'tex', 'lilypond' ) + t

        if os.environ.has_key('TMP'):
            this.__set('tmp',os.environ['TMP'],'environment')

        if not os.environ.has_key('HOME'):
            if os.environ.has_key('HOMEDRIVE') and \
                 os.environ.has_key('HOMEPATH'):
                os.environ['HOME'] = os.environ['HOMEDRIVE'] + \
                                     os.environ['HOMEPATH']
            else:
                os.environ['HOME'] = os.curdir
        

	fd =this.get_texfile_path ('titledefs.tex')
        mudefs=[]    
	line='prime the pump' # ugh
        while line:
            line=fd.readline()
            m=re.match('\\\\newcommand\*{\\\\mudela([\w]+)}',line)
            if m:
                mudefs.append(m.group(1))
	fd.close
        this.__set('titledefs', mudefs, 'init')

#
# Read rc file
#
    def rcfile(this):
	"""Read RCfile"""
        varTable = [
            #   name              set method
            #   ----              ----------
            ( 'LILYPOND',       this.setInclude ),
            ( 'LANGUAGE',       this.setLanguage ),
            ( 'LATEXHF',        this.setHeader ),
            ( 'ORIENTATION',    this.setOrientation ),
            ( 'OUTPUTDIR',      this.setOutput ),
            ( 'PAPERSIZE',      this.setPaperZize ),
            ( 'PHEIGHT',        this.setTextHeight ),
            ( 'TMP',            this.setTmp ),
            ( 'PWIDTH',         this.setLineWidth ),
            ]

	for d in [os.path.join(this.get('root'),'share','lilypond'), \
                  os.environ['HOME'], os.curdir ]:
	    file=os.path.join(d,'.lilyrc')
	    try:
		fd = open( file, 'r' )
	    except:
		continue
	    
	    line='prime the pump' # ugh
	    while line:
		line=fd.readline()
		if re.match('#.*',line):
		    continue
		m=re.search('([\w]+)=(.*)',line)
		if m:
                    for var in varTable:
                        if m.group(1) == var[0]:
                            var[1](m.group(2),'rcfile')
                            break
	    fd.close

#
# Set paper size
#
    def setPaperZize(this,size,requester):
        paperTable = [
            # regex          width    height      name
            # -----          -----    ------      ----
            ( 'a0.*',        2389,    3381,    'a0paper' ),
            ( 'a1$|a1p.*',   1690,    2389,    'a1paper' ),
            ( 'a2.*',        1194,    1690,    'a2paper' ),
            ( 'a3.*',        845,     1194,    'a3paper' ),
            ( 'a4.*',        597,     845,     'a4paper' ),
	    ( 'a5.*',        423,     597,     'a5paper' ),
            ( 'a6.*',        298,     423,     'a6paper' ),
            ( 'a7.*',        211,     298,     'a7paper' ),
            ( 'a8.*',        305,     211,     'a8paper' ),
            ( 'a9.*',        105,     305,     'a9paper' ),
            ( 'a10.*',       74,      105,     'a10paper' ),
            ( 'b0.*',        2847,    4023,    'b0paper' ),
            ( 'b1.*',        2012,    2847,    'b1paper' ),
            ( 'b2.*',        1423,    2012,    'b2paper' ),
            ( 'b3.*',        1006,    1423,    'b3paper' ),
            ( 'b4.*',        712,     1006,    'b4paper' ),
            ( 'b5.*',        503,     712,     'b5paper' ),
            ( 'archA$',      650,     867,     'archApaper' ),
            ( 'archB$',      867,     1301,    'archBpaper' ),
            ( 'archC$',      1301,    1734,    'archCpaper' ),
            ( 'archD$',      1734,    2602,    'archDpaper' ),
            ( 'archE$',      2602,    3469,    'archEpaper' ),
            ( 'flsa$|flse$', 614,     940,     'flsapaper' ),
            ( 'halfletter$', 397,     614,     'halfletterpaper' ),
            ( 'ledger$',     1229,    795,     'ledgerpaper' ),
            ( 'legal$',      614,     1012,    'legalpaper' ),
            ( 'letter$',     614,     795,     'letterpaper' ),
            ( 'note$',       542,     723,     'notepaper' )
            ]

        found=0
        for paper in paperTable:
            if re.match(paper[0],size):
                found=1
                this.__set('pagewidth',paper[1],requester)
                this.__set('pageheight',paper[2],requester)
                this.__set('papersize',paper[3],requester)
                break

        if not found:
            sys.exit('ExitBadPaper',size)

#
# set Text Height
#
    def setTextHeight(this,size,requester):
	m=re.match('([0-9][.0-9]*)(cm|mm|pt|$)',size)
	if m:
	    if m.group(2) == 'cm':
		this.__set('textheight',\
                           float(m.group(1)) * 72.27/2.54, requester )
	    elif m.group(2) == 'mm':
		this.__set('textheight',\
                           float(m.group(1)) * 72.27/25.4, requester )
	    elif m.group(2) == 'pt':
		this.__set('textheight', float(m.group(1)), requester )
	    elif m.group(2) == '':
		this.__set('textheight', float(m.group(1)), requester )
	    else:
		sys.exit('ExitBadHeight', m.group(2))
	else:		
	    sys.exit('ExitBadHeight', size)
#
# set Text Width
#
    def setLineWidth(this,size,requester):
	m=re.match('([0-9][.0-9]*)(cm|mm|pt|$)',size)
	if m:
	    if m.group(2) == 'cm':
		this.__set('linewidth', \
		float(m.group(1)) * 72.27/2.54, requester )
	    elif m.group(2) == 'mm':
		this.__set('linewidth', \
		float(m.group(1)) * 72.27/25.4, requester )
	    elif m.group(2) == 'pt':
		this.__set('linewidth', float(m.group(1)), requester )
	    elif m.group(2) == '':
		this.__set('linewidth', float(m.group(1)), requester )
	    else:
		sys.exit('ExitBadWidth', m.group(2))
	else:		
	    sys.exit('ExitBadWidth', size)
#
# Set Orientation
#
    def setOrientation(this,orient,requester):
	if orient == 'landscape' or orient == 'portrait':
	    this.__set('orientation', orient, requester )
	else:
	    sys.exit('ExitBadOrient', orient)
#
# Set Language
#
    def setLanguage(this,lang,requester):
	this.__set('language', '\\usepackage[%s]{babel}' % (lang), requester )
#
# Append Include
#
    def setInclude(this,inc, requester):
        tmp = this.get('include')
        tmp.append(inc)
        this.__set('include', tmp, requester )
#
# Set debug flag
#
    def setDebug(this,requester):
	this.__set('debug',1,requester)

#
# Clear debug flag
#
    def clearDebug(this, requester):
	this.__set('debug',0,requester)
#
# Set Keeplilypond flag
#
    def setKeeplilypond(this, requester):	
	this.__set('keeplilypond',1,requester)

#
# Clear Keeplilypond flag
#
    def clearKeeplilypond(this, requester):	
	this.__set('keeplilypond',0,requester)

#
# Set Keeply2dvi flag
#
    def setKeeply2dvi(this, requester):	
	this.__set('keeply2dvi',1,requester)
#
# Clear Keeply2dvi flag
#
    def clearKeeply2dvi(this, requester):	
	this.__set('keeply2dvi',0,requester)
#
# Set No page number flag
#
    def setNonumber(this, requester):	
	this.__set('pagenumber','\\pagestyle{empty}',requester)

#
# Clear No page number flag
#
    def clearNonumber(this, requester):	
	this.__set('pagenumber','%',requester)
#
# Set separate flag
#
    def setSeparate(this, requester):	
	this.__set('separate',1,requester)

#
# Clear separate flag
#
    def clearSeparate(this, requester):	
	this.__set('separate',0,requester)

#
# Set output directory name
#
    def setOutput(this,out,requester):
	this.__set('output',out,requester)

#
# Set latex header name
#
    def setHeader(this,head, requester):
	this.__set('header',head,requester)
#
# Set Dependencies flag to generate makefile dependencies
#
    def setDependencies(this, requester):	
	this.__set('dependencies',1,requester)

#
# Clear Dependencies flag
#
    def clearDependencies(this, requester):	
	this.__set('dependencies',0,requester)
#
# Set tmp directory
#
    def setTmp(this,dir, requester):	
	this.__set('tmp',dir,requester)
#
# Set mudela source file name
#
    def setFilename(this,file, requester):	
	this.__set('filename',file,requester)
#
# Set title commands
#
    def setTitles(this,titles, requester):	
	this.__set('titles',titles,requester)

    def addLilyOutputFiles(this,file,requester):
        tmp = this.get('lilyOutputFiles')
        tmp.append(file)
        this.__set('lilyOutputFiles',tmp,requester)

    def printProps(this):
        for key in this.__data.keys():
            print "%s <%s>:<%s>" % (key,this.get(key),
                                    this.__roverrideTable[this.__data[key][1]])

def getLilyopts():
    inc = ''	
    if len(Props.get('include')) > 0: 
        inc = '-I ' + string.join(Props.get('include'),os.pathsep)
    else:

        if Props.get('dependencies'):
            dep=' -d'
        else:
            dep=''
	return inc + dep
    return inc

def writeLilylog(contents):
    if Props.get('keeplilypond'):
        file='lilylog.' + str(os.getpid())
        output = Props.get('output')
        if output != '':
            file = os.path.join( output, file )
        try:
            fd = open( file, 'w' )
        except:
            sys.exit('ExitNoWrite', file)
        fd.write(contents)
        fd.close()

def getTeXFile(contents):
    m = re.search('^TeX output to (.+)\.tex', contents,re.M)
    if m:
        return ( m.group(1)+'.tex' )
    else:
        sys.exit('ExitNoTeXName')

def program_id ():
    return name + ' ' + version;


def mailaddress():
    try:
	return os.environ['MAILADDRESS']
    except KeyError:
	return '(address unknown)'


def identify ():
    sys.stderr.write (program_id () + '\n')

def help ():
    sys.stderr.write (
        'Generate dvi file from mudela or lilypond output\n'
        'Usage: ' + name + ' [OPTION]... [FILE]...\n'
        '\n'
        'Options:\n'
        '  -D,--debug           increase verbosity\n'
        '  -F,--headers=        name of additional LaTeX headers file\n'
        '  -H,--Height=         set paper height (points) (see manual page)\n'
        '  -I,--include=DIR     add DIR to LilyPond\'s search path\n'
        '  -K,--keeplilypond    keep lilypond output files\n'
        '  -L,--landscape       set landscape orientation\n'
        '  -N,--nonumber        switch off page numbering\n'
        '  -O,--orientation=    set orientation (obsolete - use -L instead)\n'
        '  -W,--Width=          set paper width (points) (see manual page)\n'
        '  -d,--dependencies    tell lilypond make a dependencies file\n'
        '  -h,--help            this help text\n'
        '  -k,--keeply2dvi      keep ly2dvi output files\n'
        '  -l,--language=       give LaTeX language (babel)\n'
        '  -o,--output=         set output directory\n'
        '  -p,--papersize=      give LaTeX papersize (eg. a4)\n'
        '  -s,--separate        run all files separately through LaTeX\n'
        '\n'
        'files may be (a mix of) input to or output from lilypond(1)\n'
        )


def main():
    """Generate dvi files from lilypond source/output"""

    infile = Input()
    outfile = TeXOutput()
    texInputFiles=[]

    Props.rcfile()
    (options, files) = getopt.getopt (sys.argv[1:],
                                      'DF:H:I:KLNW:dhkl:o:p:s',
                                      ['debug', 'headers=', 'Height=',
                                       'include=', 'keeplilypond', 'landscape',
                                       'nonumber', 'Width=', 'dependencies',
                                       'help', 'keeply2dvi', 'language=',
                                       'output=', 'papersize=', 'separate'])
    for opt in options:
        o = opt[0]
        a = opt[1]
        if o == '--debug' or o == '-D':
	    Props.setDebug('commandline')
        elif o == '--headers' or o == '-F':
	    Props.setHeader(a,'commandline')
        elif o == '--include' or o == '-I':
	    Props.setInclude(a,'commandline')
        elif o == '--Height' or o == '-H':
	    Props.setTextHeight(a,'commandline')
        elif o == '--keeplilypond' or o == '-K':
	    Props.setKeeplilypond('commandline')
        elif o == '--landscape' or o == '-L':
	    Props.setOrientation('landscape','commandline')
        elif o == '--nonumber' or o == '-N':
	    Props.setNonumber('commandline')
        elif o == '--Width' or o == '-W':
	    Props.setLineWidth(a,'commandline')
        elif o == '--dependencies' or o == '-d':
	    Props.setDependencies('commandline')
        elif o == '--help' or o == '-h':
            help()
            return 0
        elif o == '--keeply2dvi' or o == '-k':
	    Props.setKeeply2dvi('commandline')
        elif o == '--language' or o == '-l':
	    Props.setLanguage(a,'commandline')
        elif o == '--output' or o == '-o':
	    Props.setOutput(a,'commandline')
        elif o == '--papersize' or o == '-p':
	    Props.setPaperZize(a,'commandline')
        elif o == '--separate' or o == '-s':
	    Props.setSeparate('commandline')

    if len(files):
        for file in files:
            infile.open(file)
            type = infile.type()
            infile.close()
            if type == 'source':
                cmd = 'lilypond %s %s 2>&1' % (getLilyopts(), file)
                fd = os.popen( cmd , 'r' )
                log = fd.read()
                stat = fd.close()
                print log
                if stat:
                    sys.exit('ExitBadLily', cmd )
                texFile=getTeXFile(log)
                writeLilylog(log)
                Props.addLilyOutputFiles(texFile,'program')
                texInputFiles.append(texFile)
            else:
                texInputFiles.append(file)

        firstfile=1
        for file in texInputFiles:
            infile.open(file)
            infile.setVars() # first pass set variables
            infile.close()
            Props.printProps()
            if firstfile:
                outfile.start(file)
            else:
                outfile.next()
            outfile.write("""\
\\input{%s}
""" % (file))
            if Props.get('separate'):
                outfile.end()
            else:
                firstfile=0
        if not Props.get('separate'):
            outfile.end()
    else:
        help()
        sys.exit('ExitBadArgs','No files specified')

#
# Exit values
#
ExitTable = {
    'ExitInterupt'         : ['Ouch!', 1 ],
    'ExitBadArgs'          : ['Wrong number of arguments', 2 ],
    'ExitNotFound'         : ['File not found', 3 ],
    'ExitBadPaper'         : ['Unknown papersize', 4 ],
    'ExitBadHeight'        : ['Invalid Height specification', 5 ],
    'ExitBadWidth'         : ['Invalid Width specification', 6 ],
    'ExitBadOrient'        : ['Invalid Orientation specification', 7 ],
    'ExitNoWrite'          : ['Permission denied', 8 ],
    'ExitNoTeXName'        : ['hmm, I could not find an output file name', 9 ],
    'ExitBadLily'          : ['Lilypond failed', 10 ],
    'ExitBadLatex'         : ['Latex failed', 11 ],
    'ExitUnknown'          : ['Unknown Exit Code', 20 ],
    }

def cleanup():
    lilyfiles = []
    tmpfiles = []
    if not Props.get('keeplilypond'):
        lilyfiles = Props.get('lilyOutputFiles')
    if not Props.get('keeply2dvi'):
        tmpfiles = glob.glob('*.' + str(os.getpid()) + '.*' )
    for file in lilyfiles + tmpfiles:
        if os.path.isfile(file):
            os.remove(file)







identify()
Props = Properties()

try:
    main()

except KeyboardInterrupt:
    print ExitTable['ExitInterupt'][0]
    cleanup()
    sys.exit(ExitTable['ExitInterupt'][1])

except SystemExit, errno:
    if ExitTable.has_key(errno.args[0]):
        msg = ExitTable[errno.args[0]]
    else:
        msg = ExitTable['ExitUnknown']
    if len(errno.args) > 1:  
        sys.stderr.write( '%s: %s: %s\n' % (name, msg[0], errno.args[1]))
    else:
        sys.stderr.write( '%s %s\n' % (name, msg[0]))
    cleanup()
    sys.exit(msg[1])
else:
    cleanup()

    

                                   



