#!@PYTHON@

"""
=======================================================================
LilyPond to dvi converter

Features include Title information, paper size specification, and image
orientation.  

Usage: ly2dvi.py [OPTION]... [FILE]...
Input: LilyPond source or LilyPond generated TeX files
Output: DVI file
=======================================================================
"""

name = 'ly2dvi'
version = '0.0.6'
errorlog = ''

import sys
import os
import getopt
import re
import string
import time
import glob


class Input:
    """
    This class handles all ly2dvi.py input file methods

    Public methods:
    
    __init__()  Constructor
    open(file)  Open a .ly file or .tex file using lilyinclude path
    close()     Close current file
    type()      Determine file type .ly (input) or .tex (output)
    setVars()   Set title definitions found in .tex (output) file
    """

    #
    # Constructors
    #

    def __init__(this):
       this.__fd = None 

    #
    # open
    #
    def open(this,file):
        """
        open file and set private class variable __fd.  The search
        sequence is: current directory followed by the directories
        found in include property list.  Each directory is searched
        for file, file.ly, and file.fly.
        
        input:  file   filename
        output: void
        error:  ExitNotFound Exception
        """

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


    #
    # close
    #
    def close(this):
        """
        close file object __fd
        
        input:  void
        output: void
        error:  None
        """
        this.__fd.close()


    #
    # type
    #
    def type(this):
        """
        Determine input file type.  LilyPond source is 'input' type
        and LilyPond generated TeX file is 'output' type

        input:  void
        output: 'input' | 'output'
        error:  None
        """

        firstline = this.__fd.readline()
        this.__fd.seek(0)
        if  re.match('% Creator: GNU LilyPond [0-9]+[.0-9]+',firstline ):
            return 'output'
        else:
            return 'source'


    #
    # setVars
    #
    def setVars(this):	
        """
        Search for properties in the current input file and set the
        appropriate values.  The supported properties names are in
        local variable varTable along with the property list
        titledefs.

        input:  void
        output: None
        error:  None
        """

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
        for line in this.__fd.readlines():
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
    """
    This class handles all ly2dvi.py output file methods

    private methods:
     __mudelaDefs(opt)  Send title info to output file

    Public methods:
    __init__()  Constructor
    write(str)  Write a string to output file 
    start(file) Start the latex file
    next()      Process next output file
    end()       Finish latex file and run latex 
    """

    #
    # constructor
    #
    def __init__(this):
       this.__fd = None 
       this.__base = ''
       this.__outfile = ''

    #
    # __medelaDefs
    #
    def __mudelaDefs(this,opt):
        """
        Write titles to output

        input:  opt   Supports header and subheader output
        output: None
        error:  None
        """

        titles = Props.get('titles')
        for key in titles.keys():
            this.write('%s\\mudela%s{%s}%%\n' % (opt,key,titles[key]))

    #
    # write
    #
    def write(this,str):
        """
        Write str to current output file

        input:  str  String to write
        output: None
        error:  None
        """
        
        this.__fd.write(str)

    #
    # start
    #
    def start(this,file):
        """
        Start LaTeX file.  Calculates the horizontal and vertical
        margin using pagewidth, pageheight, linewidth, and textheight.
        Creates temporary output filename and opens it for write.
        Sends the LaTeX header information to output.  Lastly sends
        the title information to output.

        input:  file  output file name 
        output: None
        error:  None
        """

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

    #
    # next
    #
    def next(this):
        """
        Write LaTeX subheader information to support more than one
        score in a document.  Lastly send current title information to
        output.

        input:  None
        output: None
        error:  None
        """

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


    #
    # end
    #
    def end(this):
        """
        Close output file and run latex on it.

        input:  None
        output: None
        error:  ExitBadLatex Exception
        """

        outfile=this.__base + '.dvi'
        if Props.get('output') != '':
            outfile = os.path.join(Props.get('output'), outfile )
            
        this.write("""\
\\vfill\\hfill{\\LilyIdString}
\\end{document}
""")
        this.__fd.close()
        if ( os.name == 'posix' ):
            stat = os.system('latex \'\\nonstopmode \\input %s\'' %
                             (this.__outfile))
        else: # Windows shells don't eat the single quotes
            stat = os.system('latex \\nonstopmode \\input %s' %
                             (this.__outfile))
        if stat:
            sys.exit('ExitBadLatex')
        if os.path.isfile(outfile):
            os.remove(outfile)
        os.rename(this.__base + '.' + str(os.getpid()) + '.dvi', outfile)
        sys.stderr.write( '\n' + program_id() + ': dvi file name is %s\n\n'
                   % (outfile))

        if Props.get('postscript'):
            psoutfile=this.__base + '.ps'
            if Props.get('output') != '':
                psoutfile = os.path.join(Props.get('output'), psoutfile )
            stat = os.system('dvips -o %s %s 2>&1' % (psoutfile,outfile))
            if stat:
                sys.exit('ExitBadPostscript')
            



class Properties:
    """
    This class handles all ly2dvi.py property manipulation

    Public methods:
    
    __init__()  Constructor
    set<property> methods
    """

    def __init__(this):

        #
        # Following is the order of priority for property assignment.  The
        # list is organized from lowest to highest priority.  Each
        # assignment is overridden by the next requester in the list.
        #
        # Requester     Description
        # ---------     -----------
        # init          Initial default values
        # file          The values found in the lilypond generated TeX files
        # environment   Envrionment variables LILYINCLUDE, LILYPONDPREFIX
        # rcfile        $LILYPONDPREFIX/share/lilypond/.lilyrc
        # rcfile        $HOME/.lilyrc
        # rcfile        ./.lilyrc
        # commandline   command line arguments
        # 
        this.__overrideTable = {
            'init'        : 0,
            'file'        : 1,
            'environment' : 2,
            'rcfile'      : 3,
            'commandline' : 4,
            'program'     : 5
            }

        this.__roverrideTable = {} # reverse lookup used for debug
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
            'postscript'   :  [0, this.__overrideTable['init']],
            }

        #
        # Try to set root and HOME first before calling rcfile
        #
        if os.environ.has_key('LILYPONDPREFIX'):
            this.setRoot(os.environ['LILYPONDPREFIX'], 'environment')
        else:
            p=os.path.split(sys.argv[0])
            p=os.path.split(p[0])
            this.setRoot(p[0],'init')

        if not os.environ.has_key('HOME'):
            if os.environ.has_key('HOMEDRIVE') and \
                 os.environ.has_key('HOMEPATH'):
                os.environ['HOME'] = os.environ['HOMEDRIVE'] + \
                                     os.environ['HOMEPATH']
            else:
                os.environ['HOME'] = os.curdir

        this.rcfile() # Read initialization file(s)

        if os.environ.has_key('LILYINCLUDE'):
            tmp=this.get('include')
            for s in string.split(os.environ['LILYINCLUDE'],os.pathsep):
                tmp.append(s)
            this.__set('include', tmp, 'environment')    


        t = os.pathsep
	if os.environ.has_key ('TEXINPUTS'):
		t = os.environ['TEXINPUTS'] + t
        os.environ['TEXINPUTS'] = t + os.path.join(this.get('root'), 'share',
					           'lilypond', 'tex' )

        t = os.pathsep
	if os.environ.has_key ('MFINPUTS'):
		t = os.environ['MFINPUTS'] + t
        os.environ['MFINPUTS'] = t + os.path.join(this.get('root'), 'share',
					          'lilypond', 'mf' )

        if os.environ.has_key('TMP'):
            this.__set('tmp',os.environ['TMP'],'environment')

        
	fd=this.get_texfile_path ('titledefs.tex')
        mudefs=[]    

        for line in fd.readlines():
            m=re.match('\\\\newcommand\*{\\\\mudela([\w]+)}',line)
            if m:
                mudefs.append(m.group(1))
	fd.close
        this.__set('titledefs', mudefs, 'init')

    #
    # __set
    #
    def __set(this,var,value,requester):
        """
        All of the set methods call this to set a property.  If the value
        was last set by a requestor of lesser priority the new value is
        assigned, else the old value has priority and is unchanged.
        """

        if this.__overrideTable[requester] < this.__data[var][1]:
            return 0
        else:
            this.__data[var] = [value, this.__overrideTable[requester]]

    #
    # get
    #
    def get(this,var):
        """
        All of the get methods call this to get a property value.  List
        variable types are return by value to facilitate an append operation.
        """

        if var == 'include' or var == 'lilyOutputFiles':
            return this.__data[var][0][0:]  # return a copy not a ref
        else:
            return this.__data[var][0]

    #
    # get_texfile_path
    #
    def get_texfile_path (this, var):
        """
        locate and open titledefs.tex file
        """

        if os.name == 'nt':
            path = os.path.join(this.get('root'), 'share', 'lilypond',
                                'tex', var)
        else:
            path =''
            cmd =('kpsewhich tex %s %s' % (var,errorlog))
            pipe = os.popen (cmd, 'r')
            path = pipe.readline ()[:-1] # chop off \n
            return_status =  pipe.close()
            if return_status and not path:
                path = os.path.join(this.get('root'), 'share', 'lilypond',
                                    'tex', var)
	fd = open(path, 'r')
        return fd


    #
    # Read rc file
    #
    def rcfile(this):
	"""
        Read initialization file(s)
        """
        varTable = [
            #   name              set method
            #   ----              ----------
            ( 'DEBUG',          this.setDebug ),
            ( 'DEPENDENCIES',   this.setDependencies ),
            ( 'KEEPLILYPOND',   this.setKeeplilypond ),
            ( 'KEEPLY2DVI',     this.setKeeply2dvi ),
            ( 'LANGUAGE',       this.setLanguage ),
            ( 'LATEXHF',        this.setHeader ),
            ( 'LILYINCLUDE',    this.setInclude ),
            ( 'LILYPONDPREFIX', this.setRoot ),
            ( 'NONUMBER',       this.setNonumber ),
            ( 'ORIENTATION',    this.setOrientation ),
            ( 'OUTPUTDIR',      this.setOutput ),
            ( 'PAPERSIZE',      this.setPaperZize ),
            ( 'PHEIGHT',        this.setTextHeight ),
            ( 'POSTSCRIPT',     this.setPostscript ),
            ( 'PWIDTH',         this.setLineWidth ),
            ( 'SEPARATE',       this.setSeparate ),
            ( 'TMP',            this.setTmp ),
            ]

        if ( os.name == 'posix' ):
            dotFilename='.lilyrc'
        else: # Windows apps like edit choke on .lilyrc
            dotFilename='_lilyrc'

	for d in [os.path.join(this.get('root'),'share','lilypond','ly'), \
                  os.environ['HOME'], os.curdir ]:
	    file=os.path.join(d,dotFilename)
	    try:
		fd = open( file, 'r' )
	    except:
		continue
	    
            for line in fd.readlines():
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
    # setPaperZize
    #
    def setPaperZize(this,size,requester):
        """
        Set paper size properties
        """

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
    # setTextHeight
    #
    def setTextHeight(this,size,requester):
        """
        Set textheight property
        """

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
    # setLineWidth
    #
    def setLineWidth(this,size,requester):
        """
        Set linewidth propery
        """

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
    # setOrientation
    #
    def setOrientation(this,orient,requester):
        """
        Set orientation property
        """

	if orient == 'landscape' or orient == 'portrait':
	    this.__set('orientation', orient, requester )
	else:
	    sys.exit('ExitBadOrient', orient)

    #
    # setLanguage
    #
    def setLanguage(this,lang,requester):
        """
        Set language property
        """

	this.__set('language', '\\usepackage[%s]{babel}' % (lang), requester )

    #
    # setInclude
    #
    def setInclude(this,inc, requester):
        """
        Append an include path
        """

        tmp = this.get('include')
        tmp.append(inc)
        this.__set('include', tmp, requester )

    #
    # setDebug
    #
    def setDebug(this,value,requester):
        """
        Set or Clear debug flag
        """

        if int(value) == 1:
            this.__set('debug',1,requester)
        else:
            this.__set('debug',0,requester)

    #
    # setKeeplilypond
    #
    def setKeeplilypond(this, value, requester):	
        """
        Set or Clear keeplilypond flag
        """

        if int(value) == 1:
            this.__set('keeplilypond',1,requester)
        else:
            this.__set('keeplilypond',0,requester)

    #
    # setKeeply2dvi
    #
    def setKeeply2dvi(this, value, requester):	
        """
        Set or Clear keeply2dvi flag
        """

        if int(value) == 1:
            this.__set('keeply2dvi',1,requester)
        else:
            this.__set('keeply2dvi',0,requester)

    #
    # setNonumber 
    #
    def setNonumber(this, value, requester):	
        """
        Set nonumber flag
        """

        if int(value) == 1:
            this.__set('pagenumber',1,requester)
        else:
            this.__set('pagenumber',0,requester)

    #
    # setSeparate
    #
    def setSeparate(this, value, requester):	
        """
        Set or Clear separate flag
        """

        if int(value) == 1:
            this.__set('separate',1,requester)
        else:
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
    # Set or Clear Dependencies flag to generate makefile dependencies
    #
    def setDependencies(this, requester):	
        """
        Set or Clear dependencies flag
        """

        if int(value) == 1:
            this.__set('dependencies',1,requester)
        else:
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

    #
    # Set title commands
    #
    def addLilyOutputFiles(this,filelist,requester):
        """
        Add a to the lily output list
        """

        tmp = this.get('lilyOutputFiles')
        tmp = tmp + filelist
        this.__set('lilyOutputFiles',tmp,requester)

    #
    # Set/Clear postscript flag
    #
    def setPostscript(this,value,requester):
        """
        Set postscript flag
        """

        if int(value) == 1:
            this.__set('postscript',1,requester)
        else:
            this.__set('postscript',0,requester)

    #
    # Set root
    #
    def setRoot(this,path, requester):	
        """
        Set lilypond root directory
        """

        os.environ['LILYPONDPREFIX'] = path
	this.__set('root',path,requester)

    #
    # printProps
    #
    def printProps(this):
        """
        Print properties
        """
        
        for key in this.__data.keys():
            print "%s <%s>:<%s>" % (key,this.get(key),
                                    this.__roverrideTable[this.__data[key][1]])



#
# Misc functions
#

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
    texfiles=[]
    for line in string.split(contents,'\n'):
        m = re.search('^TeX output to (.+)\.\.\.', line)
        if m:
            texfiles.append(m.group(1))

    if texfiles == []:
        sys.exit('ExitNoTeXName')
    else:
        return texfiles

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
        '  -P,--postscript      generate postscript file\n'
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



#
# main
#

def main():
    """Generate dvi files from lilypond source/output"""

    infile = Input()
    outfile = TeXOutput()
    texInputFiles=[]

    (options, files) = getopt.getopt (sys.argv[1:],
                                      'DF:H:I:KLNPW:dhkl:o:p:s',
                                      ['debug', 'headers=', 'Height=',
                                       'include=', 'keeplilypond', 'landscape',
                                       'nonumber', 'Width=', 'dependencies',
                                       'help', 'keeply2dvi', 'language=',
                                       'output=', 'papersize=', 'separate',
                                       'postscript'])
    for opt in options:
        o = opt[0]
        a = opt[1]
        if o == '--debug' or o == '-D':
	    Props.setDebug(1,'commandline')
        elif o == '--headers' or o == '-F':
	    Props.setHeader(a,'commandline')
        elif o == '--include' or o == '-I':
	    Props.setInclude(a,'commandline')
        elif o == '--Height' or o == '-H':
	    Props.setTextHeight(a,'commandline')
        elif o == '--keeplilypond' or o == '-K':
	    Props.setKeeplilypond(1,'commandline')
        elif o == '--landscape' or o == '-L':
	    Props.setOrientation('landscape','commandline')
        elif o == '--nonumber' or o == '-N':
	    Props.setNonumber('commandline')
        elif o == '--Width' or o == '-W':
	    Props.setLineWidth(a,'commandline')
        elif o == '--dependencies' or o == '-d':
	    Props.setDependencies(1,'commandline')
        elif o == '--help' or o == '-h':
            help()
            return 0
        elif o == '--keeply2dvi' or o == '-k':
	    Props.setKeeply2dvi(1,'commandline')
        elif o == '--language' or o == '-l':
	    Props.setLanguage(a,'commandline')
        elif o == '--output' or o == '-o':
	    Props.setOutput(a,'commandline')
        elif o == '--papersize' or o == '-p':
	    Props.setPaperZize(a,'commandline')
        elif o == '--separate' or o == '-s':
	    Props.setSeparate(1,'commandline')
        elif o == '--postscript' or o == '-P':
	    Props.setPostscript(1,'commandline')

    if len(files):
        for file in files:
            infile.open(file)
            type = infile.type()
            infile.close()
            if type == 'source':
                cmd = 'lilypond %s %s 2>&1' % (getLilyopts(), file)
                fd = os.popen( cmd , 'r' )
                log = ''
                line=fd.readline()
                while line:
                    log = log + line
                    sys.stderr.write( line )
                    line=fd.readline()
                stat = fd.close()
                if stat:
                    sys.exit('ExitBadLily', cmd )
                texFiles=getTeXFile(log)
                writeLilylog(log)
                Props.addLilyOutputFiles(texFiles,'program')
                texInputFiles = texInputFiles + texFiles
            else:
                texInputFiles.append(file)

        firstfile=1
        for file in texInputFiles:
            infile.open(file)
            infile.setVars() # first pass set variables
            infile.close()
            if Props.get('debug'):
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
    'ExitBadPostscript'    : ['Postscript failed', 12 ],
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
    if Props.get('debug'):
        Props.printProps()
    cleanup()
    sys.exit(msg[1])
else:
    cleanup()
