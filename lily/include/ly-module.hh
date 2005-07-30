/*
  ly-module.hh -- declare  module related helper functions

  source file of the GNU LilyPond music typesetter

  (c) 2002--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef LY_MODULE_HH
#define LY_MODULE_HH

#include "lily-guile.hh"

SCM ly_make_anonymous_module (bool safe);
SCM ly_module_copy (SCM dest, SCM src);
SCM ly_module2alist (SCM mod);
SCM ly_module_lookup (SCM module, SCM sym);
SCM ly_modules_lookup (SCM modules, SCM sym, SCM);
SCM ly_module_symbols (SCM mod);
void ly_reexport_module (SCM mod);
inline bool ly_c_module_p (SCM x) { return SCM_MODULEP (x); }
SCM ly_clear_anonymous_modules ();
SCM ly_use_module (SCM mod, SCM used);

#endif /* LY_MODULE_HH */

