/*
  ly-module.hh -- declare  module related helper functions

  source file of the GNU LilyPond music typesetter

  (c) 2002--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/
#ifndef LY_MODULE_HH
#define LY_MODULE_HH

#include "lily-guile.hh"

SCM ly_make_anonymous_module (bool safe);
SCM ly_module_copy (SCM dest, SCM src);
SCM ly_module_2_alist (SCM mod);
SCM ly_module_lookup (SCM module, SCM sym);
SCM ly_modules_lookup (SCM modules, SCM sym, SCM);
SCM ly_module_symbols (SCM mod);
void ly_reexport_module (SCM mod);
inline bool ly_is_module (SCM x) { return SCM_MODULEP (x); }
SCM ly_clear_anonymous_modules ();
void clear_anonymous_modules ();
SCM ly_use_module (SCM mod, SCM used);

/* Ugh signature of scm_internal_hash_fold () is inaccurate.  */
typedef SCM (*Hash_closure_function) (GUILE_ELLIPSIS);

#define MODULE_GC_KLUDGE

#endif /* LY_MODULE_HH */

