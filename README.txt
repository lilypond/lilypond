

This is the toplevel README to LilyPond
***************************************

   LilyPond is a music typesetter.  It produces beautiful sheet music
using a high level description file as input.  LilyPond is part of the
GNU Project.

Versioning
==========

   LilyPond uses a versioning scheme similar to the Linux kernel.  In a
version "x.y.z", an even second number 'y' denotes a stable version.
For development versions 'y' is odd.  For using straightforward score
production, please use the latest stable version.  Development versions
may not produce good or nice scores.

Requirements
============

   For the compilation and running of LilyPond you need some additional
packages.  Please refer to the installation instructions.

   NOTE: If you downloaded a binary (.rpm or a W95/NT .zip file), you
don't have to compile LilyPond.

Installation
============

   For your convenience, a formatted copy of the INSTALL instructions
are in the toplevel directory, as INSTALL.txt

Documentation
=============

   The real documentation is the directory Documentation/

   If you want to read the documentation online, these are options:
   * use `.html'. Refer to INSTALL.txt for info on how to make the
     .html documentation.

   * use `.html'. Point your browser to
     `http://www.cs.uu.nl/~hanwen/lilypond/index.html'.

   * use `.dvi',  for the tutorial and reference manual. Do

            make -C Documentation/user/ dvi

   * use ASCII. Do using
          	make -C doc


Comments
========

   LilyPond is a long way from finished and polished.  We do appreciate
criticism, comments, bugreports, patches, etc.

     	   Please send your e-mail to one of the MAILING LISTS

   and _not_ to us personally.  See `Documentation/mail.texi' for more
info.

Windows 32
==========

   If you have received this file as part of a DOS/Window32 distribution
(`LilyPond-*.zip'), it is advisable to also download the source
package, since it might contain more documentation
`ftp://ftp.cs.uu.nl/pub/GNU/LilyPond/'

   If you decide to build LilyPond from source, please read the
INSTALL.txt document first, especially the Windows NT/95 section.

Caveats
=======

   If you have installed a previous version, be sure to remove old font
files, eg.,
     rm `find /var/lib/texmf/fonts -name 'feta*'`

   a script to do this for you is in `buildscripts/clean-fonts.sh'

Bugs
====

   Send bug reports to <bug-gnu-music@gnu.org>.  For help and questions
use <help-gnu-music@gnu.org> and <gnu-music-discuss@gnu.org>.  Please
consult the FAQ and installation instructions before mailing your
problems.

CDROM distributions
===================

   If you have received LilyPond on a cdrom, chances are that
development has moved some patchlevels up.  Please check the latest
version of LilyPond before reporting bugs.

