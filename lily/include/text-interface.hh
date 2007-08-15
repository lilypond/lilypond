/*
  text-interface.hh -- declare markup functions

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef TEXT_ITEM
#define TEXT_ITEM

#include "stencil.hh"

class Text_interface
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (interpret_markup, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (interpret_string, (SCM, SCM, SCM));
  static bool has_interface (Grob *);
  static bool is_markup (SCM);
};

#endif /* TEXT_ITEM */
