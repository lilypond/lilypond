#!@BASH@

# WIP - this (the guile-gnome canvas) is broken ATM.

# guile-gnome.sh -- download, compile, install g-wrap, guile-gnome,
# pango

# LilyPond has an experimental gnome canvas output backend -- hackers
# only.  This depends on rather new versions of guile-gnome, g-wrap
# and pango.

set -ex

# Where user built stuff will be installed
OPT=$HOME/usr/pkg

# What extra modules to pull (eg: EXTRA="libgnomecanvas libwnck")
EXTRA=${EXTRA-libgnomecanvas}

export AUTOMAKE=automake-1.8
export ACLOCAL=aclocal-1.8
export AUTOCONF=$(which autoconf2.50)
export AUTOHEADER=$(which autoheader2.50)

export PKG_CONFIG_PATH
export LD_LIBRARY_PATH
export GUILE_LOAD_PATH

if [ -z "$AUTOCONF" ]; then
    unset AUTOCONF
fi
if [ -z "$AUTOHEADER" ]; then
    unset AUTOHEADER
fi


# test: the name of our download and build directory
rm -rf test
mkdir test
cd test

## 1.  install gnome-devel
##     - Debian/unstable: apt-get install gnome-devel
##     - ...

## 2.  get pango CVS

PKG_CONFIG_PATH=$OPT/pango/lib/pkgconfig:$PKG_CONFIG_PATH
LD_LIBRARY_PATH=$OPT/pango/lib:$LD_LIBRARY_PATH

mkdir -p gnome/CVS
if ! pkg-config --atleast-version=1.5.1 pango; then
    cd gnome
    if [ -n "$BLOEDIGE_RAND" ]; then
	echo ":pserver:anonymous@anoncvs.gnome.org:/cvs/gnome" > CVS/Root
	echo "." > CVS/Repository
	cvs -z3 checkout -P pango
    else
        wget ftp://ftp.gtk.org/pub/gtk/v2.5/pango-1.5.2.tar.gz
	tar -zf pango-1.5.2.tar.gz
	ln -s pango-1.5.2 pango
    fi
    cd pango
    rm -rf $OPT/pango
    if [ ! -f configure ]; then
	./autogen.sh --help
    fi
    ./configure --prefix=$OPT/pango --enable-maintainer-mode --enable-gtk-doc
    make XFT_LIBS="-L/usr/lib -lXft -L/usr/X11R6/lib -lfreetype -lz -lXrender -lX11 -lfontconfig" install
    cd ../..
fi

## 3. Currently (2004-9-15) GUILE CVS works again
## PATH=/usr/bin:$PATH

if [ -d $OPT/libffi/ ]; then
    export LDFLAGS=-L$OPT/libffi/lib
    export CPPFLAGS=-I$OPT/libffi/include
fi

PKG_CONFIG_PATH=$OPT/g-wrap/lib/pkgconfig:$PKG_CONFIG_PATH
LD_LIBRARY_PATH=$OPT/g-wrap/lib:$LD_LIBRARY_PATH
GUILE_LOAD_PATH=$OPT/g-wrap/share/guile/site:$GUILE_LOAD_PATH

## 4.  get g-wrap 2.0
## note that bleeding edge (2004-9-13) g-wrap breaks guile-gnome.
if ! pkg-config --exact-version=1.9.1 g-wrap-2.0-guile; then
    if [ -n "$BLOEDIGE_RAND" ]; then
	tla register-archive a.rottmann@gmx.at--2004-main \
	    http://people.debian.org/~rotty/arch/a.rottmann@gmx.at/2004-main || true

        ## tla get a.rottmann@gmx.at--2004-main/g-wrap--tng g-wrap
        ## tla get a.rottmann@gmx.at--2004-main/g-wrap--mainline--1.9.0 g-wrap
	tla get a.rottmann@gmx.at--2004-main/g-wrap--dev--0 g-wrap
        ## ughr:
	mkdir -p g-wrap/libffi
    else
	wget http://savannah.nongnu.org/download/g-wrap/g-wrap-1.9.1.tar.gz
	tar zxf g-wrap-1.9.1.tar.gz
	ln -s g-wrap-1.9.1 g-wrap
    fi
    cd g-wrap
    
    rm -rf $OPT/g-wrap
    if [ ! -f configure ]; then
	sh autogen.sh --noconfigure
    fi    
    mkdir =build
    cd =build
    ../configure --prefix=$OPT/g-wrap --enable-maintainer-mode
    make install
    cd ../..
fi    


# not a good idea
## cp srfi-34.scm from CVS head ?  --hwn
#(cd $OPT/g-wrap/share/guile/site
# mv srfi-34.scm srfi-34.scm-g-wrap
# cp $OPT/guile/share/guile-1.7/srfi/srfi-34.scm .)

PKG_CONFIG_PATH=$OPT/guile-gnome/lib/pkgconfig:$PKG_CONFIG_PATH
LD_LIBRARY_PATH=$OPT/guile-gnome/lib:$LD_LIBRARY_PATH
GUILE_LOAD_PATH=$OPT/guile-gnome/share/guile:$GUILE_LOAD_PATH

## 5.  get guile-gnome
if ! pkg-config --atleast-version=2.5.990 guile-gnome-glib; then
    if [ -n "$BLOEDIGE_RAND" ]; then

	if false; then # rotty
	    tla register-archive guile-gnome-devel@gnu.org--2004 \
		http://people.debian.org/~rotty/arch/guile-gnome-devel@gnu.org/2004/ || true
	    tla get guile-gnome-devel@gnu.org--2004/dists--dev guile-gnome
	    cd guile-gnome
	    tla build-config -r configs/gnu.org/dev
	    cd src

            # 5a.  get extra modules (gnome canvas)
	    for i in $EXTRA; do
	        tla get guile-gnome-devel@gnu.org--2004/$i--dev $i
	    done
	else # andy
	    tla register-archive wingo@pobox.com--2004-main \
		http://ambient.2y.net/wingo/arch/wingo@pobox.com--2004-main || true
	    
	    tla get wingo@pobox.com--2004-main/guile-gnome-dists--release guile-gnome
	    cd guile-gnome
	    tla build-config -r configs/gnu.org/guile-gnome-platform-2.5.990
	    cd src
	    EXTRA="pkg atk defs glib gstreamer gtk gtksourceview libgda libglade libgnome libgnomeui pango libgnomecanvas"
	    EXTRA=

            # 5a.  get extra modules (gnome canvas)
	    for i in $EXTRA; do
		tla get wingo@pobox.com--2004-main/guile-gnome-$i--release $i
	    done

	    cd libgnomecanvas
	    #patch -p1 < ~/canvas-pats
	    cd ..
	fi

	if [ ! -f configure ]; then
	    sh autogen.sh --noconfigure
	fi
	cd ..
    else
	wget http://ambient.2y.net/wingo/tmp/guile-gnome-platform-2.5.990.tar.gz
	tar xzf guile-gnome-platform-2.5.990.tar.gz
	ln -s guile-gnome-platform-2.5.990 guile-gnome
	cd guile-gnome
	ln -s . src
	patch -p0 <<EOF
diff -p'urNx*~' -x=build ../../guile-gnome-platform-2.5.990/defs/gnome/defs/libgnomecanvas.defs ./defs/gnome/defs/libgnomecanvas.defs
--- ../../guile-gnome-platform-2.5.990/defs/gnome/defs/libgnomecanvas.defs	2004-09-12 10:52:16 +0200
+++ ./defs/gnome/defs/libgnomecanvas.defs	2004-09-15 03:08:09 +0200
@@ -760,23 +760,24 @@
   )
 )
 
-(define-method affine_relative
-  (of-object "GnomeCanvasItem")
-  (c-name "gnome_canvas_item_affine_relative")
-  (return-type "none")
-  (parameters
-    '("const-double[6]" "affine")
-  )
-)
-
-(define-method affine_absolute
-  (of-object "GnomeCanvasItem")
-  (c-name "gnome_canvas_item_affine_absolute")
-  (return-type "none")
-  (parameters
-    '("const-double[6]" "affine")
-  )
-)
+;; cannot have these overridden anymore, while still using _wrap? --jcn
+;;(define-method affine_relative
+;;  (of-object "GnomeCanvasItem")
+;;  (c-name "gnome_canvas_item_affine_relative")
+;;  (return-type "none")
+;;  (parameters
+;;    '("const-double[6]" "affine")
+;;  )
+;;)
+
+;;(define-method affine_absolute
+;;  (of-object "GnomeCanvasItem")
+;;  (c-name "gnome_canvas_item_affine_absolute")
+;;  (return-type "none")
+;;  (parameters
+;;    '("const-double[6]" "affine")
+;;  )
+;;)
 
 (define-method raise
   (of-object "GnomeCanvasItem")
diff -p'urNx*~' -x=build ../../guile-gnome-platform-2.5.990/libgnomecanvas/gnome/Makefile.am ./libgnomecanvas/gnome/Makefile.am
--- ../../guile-gnome-platform-2.5.990/libgnomecanvas/gnome/Makefile.am	2004-09-12 11:22:07 +0200
+++ ./libgnomecanvas/gnome/Makefile.am	2004-09-15 03:04:04 +0200
@@ -2,10 +2,10 @@ include \$(top_srcdir)/common.mk
 
 SUBDIRS = overrides
 
-guilegwmodule_DATA = 
+guilemodule_DATA = 
 
 if HAVE_CANVAS
-guilegwmodule_DATA += canvas.scm
+guilemodule_DATA += canvas.scm
 SUBDIRS += gw
 endif
 
diff -p'urNx*~' -x=build ../../guile-gnome-platform-2.5.990/libgnomecanvas/gnome/Makefile.in ./libgnomecanvas/gnome/Makefile.in
--- ../../guile-gnome-platform-2.5.990/libgnomecanvas/gnome/Makefile.in	2004-09-12 13:36:44 +0200
+++ ./libgnomecanvas/gnome/Makefile.in	2004-09-15 03:04:04 +0200
@@ -212,7 +212,7 @@ GUILE_LOAD_PATH := \$(PKG_PATH):\${G_WRAP_
 
 SUBDIRS = overrides \$(am__append_2)
 
-guilegwmodule_DATA = \$(am__append_1)
+guilemodule_DATA = \$(am__append_1)
 
 EXTRA_DIST = canvas.scm
 DIST_SUBDIRS = gw overrides
@@ -222,7 +222,7 @@ mkinstalldirs = \$(SHELL) \$(top_srcdir)/m
 CONFIG_HEADER = \$(top_builddir)/config.h
 CONFIG_CLEAN_FILES =
 DIST_SOURCES =
-DATA = \$(guilegwmodule_DATA)
+DATA = \$(guilemodule_DATA)
 
 
 RECURSIVE_TARGETS = info-recursive dvi-recursive pdf-recursive \\
@@ -250,23 +250,23 @@ clean-libtool:
 distclean-libtool:
 	-rm -f libtool
 uninstall-info-am:
-guilegwmoduleDATA_INSTALL = \$(INSTALL_DATA)
-install-guilegwmoduleDATA: \$(guilegwmodule_DATA)
+guilemoduleDATA_INSTALL = \$(INSTALL_DATA)
+install-guilemoduleDATA: \$(guilemodule_DATA)
 	@\$(NORMAL_INSTALL)
-	\$(mkinstalldirs) \$(DESTDIR)\$(guilegwmoduledir)
-	@list='\$(guilegwmodule_DATA)'; for p in \$\$list; do \\
+	\$(mkinstalldirs) \$(DESTDIR)\$(guilemoduledir)
+	@list='\$(guilemodule_DATA)'; for p in \$\$list; do \\
 	  if test -f "\$\$p"; then d=; else d="\$(srcdir)/"; fi; \\
 	  f="\`echo \$\$p | sed -e 's|^.*/||'\`"; \\
-	  echo " \$(guilegwmoduleDATA_INSTALL) \$\$d\$\$p \$(DESTDIR)\$(guilegwmoduledir)/\$\$f"; \\
-	  \$(guilegwmoduleDATA_INSTALL) \$\$d\$\$p \$(DESTDIR)\$(guilegwmoduledir)/\$\$f; \\
+	  echo " \$(guilemoduleDATA_INSTALL) \$\$d\$\$p \$(DESTDIR)\$(guilemoduledir)/\$\$f"; \\
+	  \$(guilemoduleDATA_INSTALL) \$\$d\$\$p \$(DESTDIR)\$(guilemoduledir)/\$\$f; \\
 	done
 
-uninstall-guilegwmoduleDATA:
+uninstall-guilemoduleDATA:
 	@\$(NORMAL_UNINSTALL)
-	@list='\$(guilegwmodule_DATA)'; for p in \$\$list; do \\
+	@list='\$(guilemodule_DATA)'; for p in \$\$list; do \\
 	  f="\`echo \$\$p | sed -e 's|^.*/||'\`"; \\
-	  echo " rm -f \$(DESTDIR)\$(guilegwmoduledir)/\$\$f"; \\
-	  rm -f \$(DESTDIR)\$(guilegwmoduledir)/\$\$f; \\
+	  echo " rm -f \$(DESTDIR)\$(guilemoduledir)/\$\$f"; \\
+	  rm -f \$(DESTDIR)\$(guilemoduledir)/\$\$f; \\
 	done
 
 # This directory's subdirectories are mostly independent; you can cd
@@ -443,7 +443,7 @@ check: check-recursive
 all-am: Makefile \$(DATA)
 installdirs: installdirs-recursive
 installdirs-am:
-	\$(mkinstalldirs) \$(DESTDIR)\$(guilegwmoduledir)
+	\$(mkinstalldirs) \$(DESTDIR)\$(guilemoduledir)
 
 install: install-recursive
 install-exec: install-exec-recursive
@@ -486,7 +486,7 @@ info: info-recursive
 
 info-am:
 
-install-data-am: install-guilegwmoduleDATA
+install-data-am: install-guilemoduleDATA
 
 install-exec-am:
 
@@ -512,7 +512,7 @@ ps: ps-recursive
 
 ps-am:
 
-uninstall-am: uninstall-guilegwmoduleDATA uninstall-info-am
+uninstall-am: uninstall-guilemoduleDATA uninstall-info-am
 
 uninstall-info: uninstall-info-recursive
 
@@ -523,7 +523,7 @@ uninstall-info: uninstall-info-recursive
 	dvi-recursive info info-am info-recursive install install-am \\
 	install-data install-data-am install-data-recursive \\
 	install-exec install-exec-am install-exec-recursive \\
-	install-guilegwmoduleDATA install-info install-info-am \\
+	install-guilemoduleDATA install-info install-info-am \\
 	install-info-recursive install-man install-recursive \\
 	install-strip installcheck installcheck-am installdirs \\
 	installdirs-am installdirs-recursive maintainer-clean \\
@@ -531,7 +531,7 @@ uninstall-info: uninstall-info-recursive
 	mostlyclean-generic mostlyclean-libtool mostlyclean-recursive \\
 	pdf pdf-am pdf-recursive ps ps-am ps-recursive tags \\
 	tags-recursive uninstall uninstall-am \\
-	uninstall-guilegwmoduleDATA uninstall-info-am \\
+	uninstall-guilemoduleDATA uninstall-info-am \\
 	uninstall-info-recursive uninstall-recursive
 
 @MK@ifneq (\$(top_srcdir),\$(top_builddir))
diff -p'urNx*~' -x=build ../../guile-gnome-platform-2.5.990/libgnomecanvas/gnome/overrides/libgnomecanvas.defs ./libgnomecanvas/gnome/overrides/libgnomecanvas.defs
--- ../../guile-gnome-platform-2.5.990/libgnomecanvas/gnome/overrides/libgnomecanvas.defs	2004-09-12 11:20:43 +0200
+++ ./libgnomecanvas/gnome/overrides/libgnomecanvas.defs	2004-09-15 03:09:42 +0200
@@ -1,63 +1,71 @@
-;; -*- scheme -*-
-;; guile-gnome
-;; Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>
-
-;; This program is free software; you can redistribute it and/or    
-;; modify it under the terms of the GNU General Public License as   
-;; published by the Free Software Foundation; either version 2 of   
-;; the License, or (at your option) any later version.              
-;;                                                                  
-;; This program is distributed in the hope that it will be useful,  
-;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
-;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
-;; GNU General Public License for more details.                     
-;;                                                                  
-;; You should have received a copy of the GNU General Public License
-;; along with this program; if not, contact:
-;;
-;; Free Software Foundation           Voice:  +1-617-542-5942
-;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
-;; Boston, MA  02111-1307,  USA       gnu@gnu.org
-
-;;; Commentary:
-;;
-;;Custom wrapper definitions.
-;;
-;;; Code:
-
-(ignore-glob  "*_get_type"
-              "_*"
-              "*_ref"
-              "*_unref"
-              "*_copy"
-              "*_free"
-              "*_newv"
-              "*_valist"
-              "*_setv"
-              "*_foreach"
-              "*_valist"
-
-              ;; only used by item implementations
-              "gnome_canvas_request_redraw*"
-
-              ;; don't deal with those yet
-              "*_svp"
-              "*_svp_*"
-              )
-
-
-
-(ignore "gnome_canvas_item_affine_relative" ;; don't deal with these affines yet
-        "gnome_canvas_item_affine_absolute"
-        "gnome_canvas_item_i2w_affine"
-        "gnome_canvas_item_i2c_affine"
-        "gnome_canvas_w2c_affine"
-
-        ;; only for use by item implementations
-        "gnome_canvas_item_request_update"
-        "gnome_canvas_set_stipple_origin"
-
-        ;; these use Art types, don't deal with them yet
-        "gnome_canvas_cap_gdk_to_art"
-        "gnome_canvas_join_gdk_to_art"
-        )
+;;;; -*- scheme -*-
+;;;; guile-gnome
+;;;; Copyright (C) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
+;;;;                    Andy Wingo <wingo at pobox dot com>
+
+;;;; This program is free software; you can redistribute it and/or
+;;;; modify it under the terms of the GNU General Public License as
+;;;; published by the Free Software Foundation; either version 2 of
+;;;; the License, or (at your option) any later version.
+;;;;
+;;;; This program is distributed in the hope that it will be useful,
+;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
+;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
+;;;; GNU General Public License for more details.
+;;;;
+;;;; You should have received a copy of the GNU General Public License
+;;;; along with this program; if not, contact:
+;;;;
+;;;; Free Software Foundation           Voice:  +1-617-542-5942
+;;;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
+;;;; Boston, MA  02111-1307,  USA       gnu@gnu.org
+
+;; I'm not sure this wrapping for affine funtctions is the Right Way,
+;; but it works.  Maybe we should have a plain affine maker: double[6]
+;; list_to_affine (SCM) or have affine_absolute take a SCM list
+;; directly?
+(define-method affine_absolute
+  (of-object "GnomeCanvasItem")
+  (c-name "_wrap_gnome_canvas_item_affine_absolute")
+  (overrides "gnome_canvas_item_affine_absolute")
+  (return-type "void")
+  (parameters
+    '("double" "x1")
+    '("double" "y2")
+    '("double" "x2")
+    '("double" "y2")
+    '("double" "x3")
+    '("double" "y3")))
+
+(define-method affine_relative
+  (of-object "GnomeCanvasItem")
+  (c-name "_wrap_gnome_canvas_item_affine_relative")
+  (overrides "gnome_canvas_item_affine_relative")
+  (return-type "void")
+  (parameters
+    '("double" "x1")
+    '("double" "y2")
+    '("double" "x2")
+    '("double" "y2")
+    '("double" "x3")
+    '("double" "y3")))
+
+(ignore-glob
+ ;; _get_type is needed/useful for the canvas, why ignore those?
+ ;;"*_get_type"
+ "_*"
+ "*_ref"
+ "*_unref"
+ "*_copy"
+ "*_free"
+ "*_newv"
+ "*_setv"
+ "*_foreach"
+ ;; "*_affine*"  this removes the overridden _wrap methods now too --jcn
+  "*i2c_affine*"
+  "*i2w_affine*"
+  "*w2c_affine*"
+ "*gdk_to_art*"
+ ;; It seems that vararg functions are skipped no matter what.
+ ;;"*_valist"
+ )

EOF
    fi
    
    rm -rf $OPT/guile-gnome
    mkdir =build
    cd =build

# Using libtool < 1.6.0 together with gcc-3.4 may trigger this problem:
#
#    If a tag has not been given, and we're using a compiler which is
#    not one of the ones with which libtool was built, attempt to
#    infer the compiler from the first word of the command line passed
#    to libtool.
#
    if [ -z "$GCC34" ]; then
    # Use libtool-1.5.6, gcc-3.{2,3} without -O2,
	CFLAGS='-O -g' ../src/configure --prefix=$OPT/guile-gnome --enable-maintainer-mode
    else
    # or use gcc-3.4 with libtool-1.6.0
	CC=$GCC34 ../src/configure --prefix=$OPT/guile-gnome --enable-maintainer-mode
    fi
    make install G_WRAP_MODULE_DIR=$OPT/g-wrap/share/guile/site
fi

# simple test -- fails atm
# guile -s ../src/libgnomecanvas/examples/canvas.scm

