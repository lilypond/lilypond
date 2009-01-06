/*
  translator-ctors.cc -- implement Translator construction

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "translator.hh"

#include "international.hh"
#include "scm-hash.hh"
#include "warn.hh"

/*
  should delete these after exit.
*/

Scheme_hash_table *global_translator_dict = 0;

LY_DEFINE (get_all_translators, "ly:get-all-translators", 0, 0, 0, (),
	   "Return a list of all translator objects that may be"
	   " instantiated.")
{
  SCM l = global_translator_dict ? global_translator_dict->to_alist () : SCM_EOL;

  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    scm_set_car_x (s, scm_cdar (s));

  return l;
}

void
add_translator (Translator *t)
{
  if (!global_translator_dict)
    global_translator_dict = new Scheme_hash_table;

  SCM k = ly_symbol2scm (t->class_name ());
  global_translator_dict->set (k, t->self_scm ());

  t->unprotect ();
}

Translator *
get_translator (SCM sym)
{
  SCM v = SCM_BOOL_F;
  if (global_translator_dict)
    global_translator_dict->try_retrieve (sym, &v);

  if (v == SCM_BOOL_F)
    error (_f ("unknown translator: `%s'", ly_symbol2string (sym).c_str ()));

  return unsmob_translator (v);
}

