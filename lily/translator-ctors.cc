/*
  translator-ctors.cc -- implement Translator construction

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator.hh"
#include "dictionary.hh"
#include "warn.hh"

/*
  should delete these after exit.
 */

/*
  UGH. Dictionary is deprecated
 */
Dictionary<Translator*> *global_translator_dict=0;

LY_DEFINE(get_all_translators,"ly:get-all-translators", 0, 0, 0,  (),
	  "Return an list of a all translator objects that may be instantiated
during a lilypond run.")
{
  SCM l = SCM_EOL;
  for (std::map<String,Translator*>::const_iterator (ci (global_translator_dict->begin()));
       ci != global_translator_dict->end (); ci++)
    {
      l = scm_cons ((*ci).second->self_scm (), l);
    }
  return l;
}

void
add_translator (Translator *t)
{
  if (!global_translator_dict)
    global_translator_dict = new Dictionary<Translator*>;

 (*global_translator_dict)[classname (t)] = t;
}

Translator*
get_translator (String s)
{
  if (global_translator_dict->elem_b (s))
    {
	Translator* t = (*global_translator_dict)[s];
	return t;
    }

  error (_f ("unknown translator: `%s'", s));
  return 0;
}

