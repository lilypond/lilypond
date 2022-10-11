/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "translator.hh"
#include "lily-imports.hh"
#include "ly-scm-list.hh"
#include "international.hh"
#include "scm-hash.hh"
#include "warn.hh"
#include "protected-scm.hh"

SCM
Translator_creator::call (SCM ctx)
{
  return (allocate_ (LY_ASSERT_SMOB (Context, ctx, 1)))->unprotect ();
}

Protected_scm global_translator_dict;
Protected_scm global_translator_dict_rev;

LY_DEFINE (get_all_translators, "ly:get-all-translators", 0, 0, 0, (),
           R"(
Return a list of all translator objects that may be instantiated.
           )")
{
  Scheme_hash_table *dict = unsmob<Scheme_hash_table> (global_translator_dict);
  SCM l = dict ? dict->to_alist () : SCM_EOL;

  for (SCM &s : as_ly_scm_list (l))
    s = scm_cdr (s);

  return l;
}

void
add_translator_creator (SCM creator, SCM name, SCM description)
{
  Scheme_hash_table *dict = unsmob<Scheme_hash_table> (global_translator_dict);
  if (!dict)
    {
      global_translator_dict = Scheme_hash_table::make_smob ();
      global_translator_dict_rev = scm_make_weak_key_hash_table (to_scm (119));
      dict = unsmob<Scheme_hash_table> (global_translator_dict);
    }
  dict->set (name, creator);
  scm_hashq_set_x (global_translator_dict_rev, creator,
                   scm_cons (name, description));
}

LY_DEFINE (ly_translator_name, "ly:translator-name", 1, 0, 0, (SCM creator),
           R"(
Return the type name of the translator definition @var{creator}.  The name is a
symbol.
           )")
{
  SCM res = global_translator_dict_rev.is_bound ()
              ? scm_hashq_ref (global_translator_dict_rev, creator, SCM_BOOL_F)
              : SCM_BOOL_F;
  SCM_ASSERT_TYPE (scm_is_pair (res), creator, SCM_ARG1, __FUNCTION__,
                   "translator definition");
  return scm_car (res);
}

LY_DEFINE (ly_translator_description, "ly:translator-description", 1, 0, 0,
           (SCM creator),
           R"(
Return an alist of properties of translator definition @var{creator}.
           )")
{
  SCM res = global_translator_dict_rev.is_bound ()
              ? scm_hashq_ref (global_translator_dict_rev, creator, SCM_BOOL_F)
              : SCM_BOOL_F;
  SCM_ASSERT_TYPE (scm_is_pair (res), creator, SCM_ARG1, __FUNCTION__,
                   "translator definition");
  return scm_cdr (res);
}

LY_DEFINE (ly_register_translator, "ly:register-translator", 2, 1, 0,
           (SCM creator, SCM name, SCM description),
           R"(
Register a translator @var{creator} (usually a descriptive alist or a
function/closure returning one when given a context argument) with the given
symbol @var{name} and the given @var{description} alist.
           )")
{
  SCM_ASSERT_TYPE (ly_is_procedure (creator) || scm_is_pair (creator), creator,
                   SCM_ARG1, __FUNCTION__, "translator creator");
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);
  if (SCM_UNBNDP (description))
    description = SCM_EOL;
  else
    LY_ASSERT_TYPE (ly_is_list, description, 3);
  add_translator_creator (creator, name, description);
  return SCM_UNSPECIFIED;
}

SCM
get_translator_creator (SCM sym)
{
  SCM v = SCM_BOOL_F;
  Scheme_hash_table *dict = unsmob<Scheme_hash_table> (global_translator_dict);
  if (dict)
    dict->try_retrieve (sym, &v);

  if (scm_is_false (v))
    {
      warning (
        _f ("unknown translator: `%s'", ly_symbol2string (sym).c_str ()));
    }

  return v;
}
