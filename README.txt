

   )

This is the toplevel README to LilyPond
***************************************

   LilyPond is a music typesetter.  It produces beautiful sheet music
using a high level description file as input.  LilyPond is part of the
GNU Project.

versioning
==========

   LilyPond uses a versioning scheme similar to the Linux kernel.  In a
version "x.y.z", an even second number 'y' denotes a stable version.
For development versions 'y' is odd.  For using straightforward score
production, please use the latest stable version.  Development versions
may not produce good or nice scores.

requirements
============

   For the compilation and running of LilyPond you need some additional
packages.  Please refer to the installation instructions.

   NOTE: If you downloaded a binary (.rpm or a W95/NT .zip file), then
you don't have to compile LilyPond.

installation
============

   For your convenience, a formatted copy of the INSTALL instructions
are in the toplevel directory, as INSTALL.txt

   The process is fairly straightforward, but chances are that you have
to specify directories for TeX to `configure': this is done with the
options `--enable-tex-dir' and `--enable-mf-dir'

documentation
=============

   The real documentation is the directory Documentation/

   If you want to read the documentation online, these are options:
   * use `.html'. Refer to INSTALL.txt for info on how to make the
     .html documentation.

   * use `.html'. Point your browser to
     `http://www.cs.uu.nl/~hanwen/lilypond/index.html'.

   * use `.dvi',  for the tutorial and reference manual. Do

            make -C Documentation/tex/ dvi

   * use ASCII. Do using

          	make -C Documentation/

     The tutorial and the reference manual  can not be made in ASCII, as
     they contain graphics.

comments
========

   LilyPond is a long way from finished and polished.  We do appreciate
criticism, comments, bugreports, patches, etc.

     	   Please send your e-mail to one of the MAILING LISTS

   and _not_ to us personally.  See `Documentation/links.yo' for more
info.

windows 32
==========

   If you have received this file as part of a DOS/Window32 distribution
(LilyPond-*.zip), then it is advisable to also download the source
package, since it might contain more documentation
`ftp://ftp.cs.uu.nl/pub/GNU/LilyPond/'

   If you decide to build LilyPond from source, please read the
INSTALL.txt document first, especially the Windows NT/95 section.

caveats
=======

   * Please read the file BUGS for some ugly bugs.

   * If you have installed a previous version, be sure to remove old
font files, eg
     rm `find /var/lib/texmf/fonts -name 'feta*'`

   a script to do this for you is in `buildscripts/clean-fonts.sh'

bugs
====

   Send bug reports to bug-gnu-music@gnu.org.  For help and questions
use help-gnu-music@gnu.org and gnu-music-discuss@gnu.org.  Please
consult the faq before mailing your problems.

cdrom distributions
===================

   if you have received LilyPond on a cdrom, chances are that
development has moved a some patchlevels up.  Please check the latest
version of LilyPond before reporting bugs.

