/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "translator.hh"

#include "international.hh"
#include "scm-hash.hh"
#include "warn.hh"
#include "protected-scm.hh"

/*
  should delete these after exit.
*/

Protected_scm global_translator_dict;

LY_DEFINE (get_all_translators, "ly:get-all-translators", 0, 0, 0, (),
           "Return a list of all translator objects that may be"
           " instantiated.")
{
  Scheme_hash_table *dict = unsmob<Scheme_hash_table> (global_translator_dict);
  SCM l = dict ? dict->to_alist () : SCM_EOL;

  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    scm_set_car_x (s, scm_cdar (s));

  return l;
}

void
add_translator (Translator *t)
{
  Scheme_hash_table *dict = unsmob<Scheme_hash_table> (global_translator_dict);
  if (!dict)
    {
      global_translator_dict = Scheme_hash_table::make_smob ();
      dict = unsmob<Scheme_hash_table> (global_translator_dict);
    }

  SCM k = ly_symbol2scm (t->class_name ());
  dict->set (k, t->unprotect ());
}

Translator *
get_translator (SCM sym)
{
  SCM v = SCM_BOOL_F;
  Scheme_hash_table *dict = unsmob<Scheme_hash_table> (global_translator_dict);
  if (dict)
    dict->try_retrieve (sym, &v);

  if (scm_is_false (v))
    {
      warning (_f ("unknown translator: `%s'", ly_symbol2string (sym).c_str ()));
      return 0;
    }

  return unsmob<Translator> (v);
}
