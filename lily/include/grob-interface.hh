/*
  interface.hh -- declare Interface

  source file of the GNU LilyPond music typesetter

  (c) 2002--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef INTERFACE_HH
#define INTERFACE_HH

#include "lily-guile.hh"

#define DECLARE_GROB_INTERFACE() \
  static SCM interface_symbol_;	   \
  static bool has_interface (Grob*)

#define ADD_INTERFACE(cl, b, c)				\
  SCM cl::interface_symbol_; \
  bool cl::has_interface (Grob *me)				\
  {								\
    return me->internal_has_interface (interface_symbol_);	\
  }								\
  void cl ## _init_ifaces ()					\
  {								\
    cl::interface_symbol_ = add_interface (#cl, b, c);		\
  }								\
  ADD_SCM_INIT_FUNC (cl ## ifaces, cl ## _init_ifaces);

SCM add_interface (char const *cxx_name,
		    char const *descr,
		    char const *vars);

SCM ly_add_interface (SCM, SCM, SCM);
void internal_add_interface (SCM, SCM, SCM);
SCM ly_all_grob_interfaces ();

#endif /* INTERFACE_HH */

