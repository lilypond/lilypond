/*
  ly-modules.hh -- declare  module related helper functions 

 source file of the GNU LilyPond music typesetter

 (c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef LY_MODULES_HH
#define LY_MODULES_HH

#include "lily-guile.hh"

SCM ly_make_anonymous_module ();
void ly_copy_module_variables (SCM dest, SCM src);
SCM ly_module_to_alist (SCM mod);
SCM ly_module_lookup (SCM module, SCM sym);
SCM ly_module_symbols (SCM mod);
void  ly_reexport_module (SCM mod);
inline bool ly_module_p (SCM x) { return SCM_MODULEP(x); }
void ly_clear_anonymous_modules ();

#endif /* LY_MODULES_HH */

