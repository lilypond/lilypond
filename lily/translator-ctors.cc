/*
  translator-ctors.cc -- implement Translator construction

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"
#include "translator.hh"
#include "dictionary.hh"
#include "debug.hh"

/*
  should delete these after exit.
 */

Dictionary<Translator*> *global_translator_dict_p=0;

void
add_translator (Translator *t)
{
  if (!global_translator_dict_p)
    global_translator_dict_p = new Dictionary<Translator*>;

  global_translator_dict_p->elem (classname (t)) = t;
}

Translator*
get_translator_l (String s)
{
  if (global_translator_dict_p->elem_b (s))
    {
//      return (*global_translator_dict_p)[s];
	Translator* t = (*global_translator_dict_p)[s];
	return t;
    }

  error (_f ("unknown translator `%s\'", s));
  return 0;
}

