/*
  graphical-lisp-element.hh -- declare Graphical_lisp_element

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef GRAPHICAL_LISP_ELEMENT_HH
#define GRAPHICAL_LISP_ELEMENT_HH
#error
#include "lily-guile.hh"
#include "lily-proto.hh"
#include "string.hh"

#define virtual
#define static
#include "virtual-methods.hh"

class Graphical_lisp_element 
{
public:
  DECLARE_MY_RUNTIME_TYPEINFO;

  Graphical_lisp_element (String);

  void* access (String);
  void call (String, void*);

private:
  String type_str_;
};

#undef virtual
#undef static

#endif // GRAPHICAL_LISP_ELEMENT_HH

