/*
  translator-ctors.cc -- implement Translator construction

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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


SCM
ly_get_all_translators ()
{
  SCM l = SCM_EOL;
  for (std::map<String,Translator*>::const_iterator (ci (global_translator_dict_p->begin()));
       ci != global_translator_dict_p->end (); ci++)
    {
      l = scm_cons ((*ci).second->self_scm (), l);
    }
  return l;
}

static void
all_trans_init()
{
  scm_c_define_gsubr ("ly-get-all-translators", 0, 0, 0, (Scheme_function_unknown) ly_get_all_translators);
}

ADD_SCM_INIT_FUNC(all_trans_init,all_trans_init);

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

