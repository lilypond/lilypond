/*
  interface.hh -- declare Interface

  source file of the GNU LilyPond music typesetter

  (c) 2002--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef INTERFACE_HH
#define INTERFACE_HH

#include "lily-guile.hh"

#define ADD_INTERFACE(cl, a, b, c)				\
  bool cl::has_interface (Grob *me)				\
  {								\
    return me->internal_has_interface (ly_symbol2scm (a));	\
  }								\
  void cl ## _init_ifaces ()					\
  {								\
    add_interface (a, b, c);					\
  }								\
  ADD_SCM_INIT_FUNC (cl ## ifaces, cl ## _init_ifaces);

void add_interface (char const *symbol,
		    char const *descr,
		    char const *vars);

SCM ly_add_interface (SCM, SCM, SCM);
SCM ly_all_grob_interfaces ();

#endif /* INTERFACE_HH */

