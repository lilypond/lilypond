/*
  translator-ctors.cc -- implement Translator construction

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator.hh"
#include "dictionary.hh"
#include "debug.hh"

/*
  should delete these after exit.
 */

/*
  UGH. Dictionary is deprecated
 */
Dictionary<Translator*> *global_translator_dict_p=0;

void
add_translator (Translator *t)
{
  if (!global_translator_dict_p)
    global_translator_dict_p = new Dictionary<Translator*>;

  (*global_translator_dict_p)[classname (t)] = t;
}

Translator*
get_translator_l (String s)
{
  if (global_translator_dict_p->elem_b (s))
    {
	Translator* t = (*global_translator_dict_p)[s];
	return t;
    }

  error (_f ("unknown translator: `%s'", s));
  return 0;
}

