# UGH.  GNU make comes with implicit rules.
# We don't want any of them, and can't force users to run
# --no-builtin-rules

.SUFFIXES:

#Compiling C programs
#    `N.o' is made automatically from `N.c' with a command of the form
#     `$(CC) -c $(CPPFLAGS) $(CFLAGS)'.

%.o: %.c

# Compiling C++ programs
#      `N.o' is made automatically from `N.cc' or `N.C' with a command of
#      the form `$(CXX) -c $(CPPFLAGS) $(CXXFLAGS)'.  We encourage you to
#      use the suffix `.cc' for C++ source files instead of `.C'.

%.o: %.cc

# Compiling Pascal programs
#      `N.o' is made automatically from `N.p' with the command `$(PC) -c
#      $(PFLAGS)'.

%.o: %.p

# Compiling Fortran and Ratfor programs
#      `N.o' is made automatically from `N.r', `N.F' or `N.f' by running
#      the Fortran compiler.  The precise command used is as follows:

#     `.f'
#           `$(FC) -c $(FFLAGS)'.

%.o: %.f

#     `.F'
#           `$(FC) -c $(FFLAGS) $(CPPFLAGS)'.

%.o: %.F

#     `.r'
#           `$(FC) -c $(FFLAGS) $(RFLAGS)'.

%.o: %.r

# Preprocessing Fortran and Ratfor programs
#      `N.f' is made automatically from `N.r' or `N.F'.  This rule runs
#      just the preprocessor to convert a Ratfor or preprocessable
#      Fortran program into a strict Fortran program.  The precise
#      command used is as follows:

#     `.F'
#           `$(FC) -F $(CPPFLAGS) $(FFLAGS)'.

%.f: %.F

#     `.r'
#           `$(FC) -F $(FFLAGS) $(RFLAGS)'.

%.f: %.r

# Compiling Modula-2 programs
#      `N.sym' is made from `N.def' with a command of the form `$(M2C)
#      $(M2FLAGS) $(DEFFLAGS)'.  `N.o' is made from `N.mod'; the form is:
#      `$(M2C) $(M2FLAGS) $(MODFLAGS)'.

%.sym: %.def
%.o: %.mod

# Assembling and preprocessing assembler programs
#      `N.o' is made automatically from `N.s' by running the assembler,
#      `as'.  The precise command is `$(AS) $(ASFLAGS)'.

%.o: %.s

#      `N.s' is made automatically from `N.S' by running the C
#      preprocessor, `cpp'.  The precise command is `$(CPP) $(CPPFLAGS)'.

%.s: %.S

# Linking a single object file
#      `N' is made automatically from `N.o' by running the linker
#      (usually called `ld') via the C compiler.  The precise command
#      used is `$(CC) $(LDFLAGS) N.o $(LOADLIBES) $(LDLIBS)'.

%: %.o

#      This rule does the right thing for a simple program with only one
#      source file.  It will also do the right thing if there are multiple
#      object files (presumably coming from various other source files),
#      one of which has a name matching that of the executable file.
#      Thus,

#           x: y.o z.o

#      when `x.c', `y.c' and `z.c' all exist will execute:

#           cc -c x.c -o x.o
#           cc -c y.c -o y.o
#           cc -c z.c -o z.o
#           cc x.o y.o z.o -o x
#           rm -f x.o
#           rm -f y.o
#           rm -f z.o

#      In more complicated cases, such as when there is no object file
#      whose name derives from the executable file name, you must write
#      an explicit command for linking.

#      Each kind of file automatically made into `.o' object files will
#      be automatically linked by using the compiler (`$(CC)', `$(FC)' or
#      `$(PC)'; the C compiler `$(CC)' is used to assemble `.s' files)
#      without the `-c' option.  This could be done by using the `.o'
#      object files as intermediates, but it is faster to do the
#      compiling and linking in one step, so that's how it's done.

# Yacc for C programs
#      `N.c' is made automatically from `N.y' by running Yacc with the
#      command `$(YACC) $(YFLAGS)'.

%.c: %.y

# Lex for C programs
#      `N.c' is made automatically from `N.l' by by running Lex.  The
#      actual command is `$(LEX) $(LFLAGS)'.

%.c: %.l

# Lex for Ratfor programs
#      `N.r' is made automatically from `N.l' by by running Lex.  The
#      actual command is `$(LEX) $(LFLAGS)'.

%.r: %.l

#      The convention of using the same suffix `.l' for all Lex files
#      regardless of whether they produce C code or Ratfor code makes it
#      impossible for `make' to determine automatically which of the two
#      languages you are using in any particular case.  If `make' is
#      called upon to remake an object file from a `.l' file, it must
#      guess which compiler to use.  It will guess the C compiler, because
#      that is more common.  If you are using Ratfor, make sure `make'
#      knows this by mentioning `N.r' in the makefile.  Or, if you are
#      using Ratfor exclusively, with no C files, remove `.c' from the
#      list of implicit rule suffixes with:

#           .SUFFIXES:
#           .SUFFIXES: .o .r .f .l ...

# Making Lint Libraries from C, Yacc, or Lex programs
#      `N.ln' is made from `N.c' by running `lint'.  The precise command
#      is `$(LINT) $(LINTFLAGS) $(CPPFLAGS) -i'.  The same command is
#      used on the C code produced from `N.y' or `N.l'.

%.ln: %.c

# TeX and Web
#      `N.dvi' is made from `N.tex' with the command `$(TEX)'.  `N.tex'
#      is made from `N.web' with `$(WEAVE)', or from `N.w' (and from
#      `N.ch' if it exists or can be made) with `$(CWEAVE)'.  `N.p' is
#      made from `N.web' with `$(TANGLE)' and `N.c' is made from `N.w'
#      (and from `N.ch' if it exists or can be made) with `$(CTANGLE)'.

%.dvi: %.tex
%.tex: %.web
%.tex: %.w
%.tex: %.ch
%.p: %.web
%.c: %.w
%.w: %.ch

# Texinfo and Info
#      `N.dvi' is made from `N.texinfo', `N.texi', or `N.txinfo', with
#      the command `$(TEXI2DVI) $(TEXI2DVI_FLAGS)'.  `N.info' is made from
#      `N.texinfo', `N.texi', or `N.txinfo', with the command
#      `$(MAKEINFO) $(MAKEINFO_FLAGS)'.

%.dvi: %.texinfo
%.dvi: %.texi
%.dvi: %.txinfo

%.info: %.texinfo
%.info: %.texi
%.info: %.txinfo

# RCS
#      Any file `N' is extracted if necessary from an RCS file named
#      either `N,v' or `RCS/N,v'.  The precise command used is
#      `$(CO) $(COFLAGS)'.  `N' will not be extracted from RCS if it
#      already exists, even if the RCS file is newer.  The rules for RCS
#      are terminal (*note Match-Anything Pattern Rules: Match-Anything
#      Rules.), so RCS files cannot be generated from another source;
#      they must actually exist.

%: %,v
%: RCS/%,v

# SCCS
#      Any file `N' is extracted if necessary from an SCCS file named
#      either `s.N' or `SCCS/s.N'.  The precise command used is
#      `$(GET) $(GFLAGS)'.  The rules for SCCS are terminal (*note
#      Match-Anything Pattern Rules: Match-Anything Rules.), so SCCS
#      files cannot be generated from another source; they must actually
#      exist.

%: s.%
%: SCCS/s.%

#      For the benefit of SCCS, a file `N' is copied from `N.sh' and made
#      executable (by everyone).  This is for shell scripts that are
#      checked into SCCS.  Since RCS preserves the execution permission
#      of a file, you do not need to use this feature with RCS.

%: %.sh

#      We recommend that you avoid using of SCCS.  RCS is widely held to
#      be superior, and is also free.  By choosing free software in place
#      of comparable (or inferior) proprietary software, you support the
#      free software movement.

#    Usually, you want to change only the variables listed in the table
# above, which are documented in the following section.

#    However, the commands in built-in implicit rules actually use
# variables such as `COMPILE.c', `LINK.p', and `PREPROCESS.S', whose
# values contain the commands listed above.

#    `make' follows the convention that the rule to compile a `.X' source
# file uses the variable `COMPILE.X'.  Similarly, the rule to produce an
# executable from a `.X' file uses `LINK.X'; and the rule to preprocess a
# `.X' file uses `PREPROCESS.X'.

#    Every rule that produces an object file uses the variable
# `OUTPUT_OPTION'.  `make' defines this variable either to contain `-o
# $@', or to be empty, depending on a compile-time option.  You need the
# `-o' option to ensure that the output goes into the right file when the
# source file is in a different directory, as when using `VPATH' (*note
# Directory Search::).  However, compilers on some systems do not accept
# a `-o' switch for object files.  If you use such a system, and use
# `VPATH', some compilations will put their output in the wrong place.  A
# possible workaround for this problem is to give `OUTPUT_OPTION' the
# value `; mv $*.o $@'.


