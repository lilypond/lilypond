/*
  translator-ctors.cc -- implement Translator construction

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "translator.hh"
#include "dictionary.hh"
#include "debug.hh"

Dictionary<Translator*> *global_translator_dict_p=0;

void
add_translator (Translator *t)
{
  if (!global_translator_dict_p)
    global_translator_dict_p = new Dictionary<Translator*>;

  global_translator_dict_p->elem (t->name ()) = t;
}

Translator*
get_translator_l (String s)
{
  if (global_translator_dict_p->elt_b (s))
    {
      return (*global_translator_dict_p)[s];
    }

  error (_("Unknown translator `") + s +"\'");
  return 0;
}

Array<Translator_ctor> *ctor_global_static_arr_p_;


/*
  Very special greetings go out to Steve Jobs for creating a system 
  that doesn't handle global construction correctly.
 */
void
add_constructor (Translator_ctor c)
{
  if (!ctor_global_static_arr_p_)
    ctor_global_static_arr_p_ = new Array<Translator_ctor>;
  ctor_global_static_arr_p_->push (c);
}

void
call_constructors ()
{
  for (int i=0; i < ctor_global_static_arr_p_->size (); i++)
    add_translator (ctor_global_static_arr_p_->elem (i) ());
}
