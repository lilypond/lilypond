

This is the toplevel README to LilyPond
***************************************

   LilyPond is a music typesetter.  It produces beautiful sheet music
using a high level description file as input.  LilyPond is part of the
GNU Project.

Versioning
==========

   LilyPond uses a versioning scheme similar to the Linux kernel.  In a
version "x.y.z", an even second number 'y' denotes a stable version.
For development versions 'y' is odd.

   Version 1.4 is the latest stable release.

Compilation
===========

   For compiling and running LilyPond please refer to the installation
instructions.  These instructions can be found when you unpack lilypond,
as `lilypond-x.y.z/INSTALL.txt'.

   NOTE: If you downloaded a binary (.rpm or a W95/NT .zip file), you
don't have to compile LilyPond.

Documentation
=============

   The documentation is available online at `http://www.lilypond.org/'.
You can also build it locally. This does require a working LilyPond
binary.  The following formats are supported:
   * HTML. Refer to INSTALL.txt for information on how to make the HTML
     documentation.

   * `.dvi',  for the tutorial and reference manual. Do

           make -C Documentation/user/ dvi
     The printable documentation is left in `Documentation/user/out/'.

   * paper. Create the .dvi documentation, and print it with dvips.


Comments
========

   LilyPond is a long way from finished and polished, so please send
your criticism, comments, bugreports, patches, etc., to the mailing
list, _not_ to us personally.

   We have the following mailing lists:

   * info-lilypond@gnu.org
     (http://mail.gnu.org/mailman/listinfo/info-lilypond) is a
     low-volume list for information on the GNU Music project.
     This list is moderated; ask     David R. Linn <drl@gnu.org> or
     Han-Wen <hanwen@cs.uu.nl> to send announcements for this list.

   * lilypond-devel@gnu.org
     (http://mail.gnu.org/mailman/listinfo/lilypond-devel)   For
     discussions about developing lilypond, in particular the unstable
     series.

   * lilypond-user@gnu.org
     (http://mail.gnu.org/mailman/listinfo/lilypond-user)   For
     discussions about using lilypond, in particular the stable series.

   * bug-lilypond@gnu.org
     (http://mail.gnu.org/mailman/listinfo/bug-lilypond) If you have
     bugreports, you should send them to this list.

Windows 32
==========

   If you have received this file as part of a DOS/Window32 distribution
(`LilyPond-*.zip'), it is advisable to also download the source package
(ftp://ftp.cs.uu.nl/pub/GNU/LilyPond/), since it might contain more
documentation

Caveats
=======

   If you have installed a previous version, be sure to remove old font
files, eg.,
     rm `find /var/lib/texmf/fonts -name 'feta*'`

   a script to do this for you is in `buildscripts/out/clean-fonts'

Bugs
====

   Send bug reports to <bug-lilypond@gnu.org>.  For help and questions
use <lilypond-user@gnu.org>.  Please consult the FAQ and installation
instructions before mailing your problems.

