/*
  text-interface.hh -- declare markup functions

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef TEXT_ITEM
#define TEXT_ITEM

#include "stencil.hh"
#include "grob-interface.hh"


class Text_interface
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (interpret_markup, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (interpret_string, (SCM, SCM, SCM));
  DECLARE_GROB_INTERFACE();
  static bool is_markup (SCM);
  static bool is_markup_list (SCM);
};

#endif /* TEXT_ITEM */
