/*
  graphical-lisp-element.cc -- implement Graphical_lisp_element

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "graphical-lisp-element.hh"
#include "string.hh"
#include "debug.hh"

#undef IMPLEMENT_STATIC_NAME
#define IMPLEMENT_STATIC_NAME(c)\
char const* c::static_name ()\
{ return type_str_.ch_C (); }\
size_t c::static_class_size () { return sizeof (c); }

IMPLEMENT_IS_TYPE_B (Graphical_lisp_element);

Graphical_lisp_element::Graphical_lisp_element (String str)
{
  type_str_ = str;
}

void*
Graphical_lisp_element::access (String str)
{
  SCM scm;
//  scm = gh_cons (gh_str02scm (str.ch_C ()));
//  scm = gh_cons (gh_symbol2scm (str.ch_C ()));
  return 0;
}

void
Graphical_lisp_element::call (String str, void* p)
{
//  gh_apply (str.ch_C (), SCM_EOL);
//  gh_apply (str.ch_C (), SCM_EOL);

// mm, common lisp only?
//  String ptr = to_str (" \\%x", p);
  String ptr = to_str (" '%x", p);
  str = "(" + str + ptr + ")";
  gh_eval_str (str.ch_l ());
//  gh_eval_str ("(add-column 0)");
}
