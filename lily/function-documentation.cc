/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "std-string.hh"
#include "lily-guile.hh"
#include "warn.hh"

/* type predicates. */
#include "global-context.hh"
#include "input.hh"
#include "item.hh"
#include "music.hh"
#include "music-function.hh"
#include "paper-score.hh"
#include "performance.hh"
#include "protected-scm.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "transform.hh"
#include "unpure-pure-container.hh"

#include <cstring>
#include <map>

using std::map;
using std::string;

static Protected_scm doc_hash_table;

void
ly_check_name (const string &cxx, const string &scm_name)
{
  string mangle = mangle_cxx_identifier (cxx);
  if (mangle != scm_name)
    {
      programming_error ("wrong cxx name: " + mangle + ", " + cxx + ", " + scm_name);
    }
}

void
ly_add_function_documentation (SCM func,
                               const string &fname,
                               const string &varlist,
                               const string &doc)
{
  if (doc == "")
    return;

  if (!doc_hash_table.is_bound ())
    doc_hash_table = scm_c_make_hash_table (59);

  string s = string (" - ") + "LilyPond procedure: " + fname + " " + varlist
             + "\n" + doc;

  scm_set_procedure_property_x (func, ly_symbol2scm ("documentation"),
                                ly_string2scm (s));
  SCM entry = scm_cons (ly_string2scm (varlist), ly_string2scm (doc));
  scm_hashq_set_x (doc_hash_table, ly_symbol2scm (fname.c_str ()), entry);
}

LY_DEFINE (ly_get_all_function_documentation, "ly:get-all-function-documentation",
           0, 0, 0, (),
           "Get a hash table with all LilyPond Scheme extension functions.")
{
  return doc_hash_table;
}

map<void *, string> type_names;

void
ly_add_type_predicate (void *ptr,
                       const string &name)
{
  type_names[ptr] = name;
}

string
predicate_to_typename (void *ptr)
{
  if (type_names.find (ptr) == type_names.end ())
    {
      programming_error ("Unknown type predicate");
      return "unknown type";
    }
  else
    return type_names[ptr];
}

void
init_func_doc ()
{
  ly_add_type_predicate ((void *) &is_scm<Direction>, "direction");
  ly_add_type_predicate ((void *) &scm_is_real, "real number");
  ly_add_type_predicate ((void *) &is_scm<Offset>, "pair of reals");
  ly_add_type_predicate ((void *) &ly_is_port, "port");
  ly_add_type_predicate ((void *) &ly_cheap_is_list, "list");
  ly_add_type_predicate ((void *) &unsmob<Global_context>, "Global_context");
  ly_add_type_predicate ((void *) &unsmob<Paper_score>, "Paper_score");
  ly_add_type_predicate ((void *) &unsmob<Performance>, "Performance");
  ly_add_type_predicate ((void *) &is_scm<Axis>, "axis");
  ly_add_type_predicate ((void *) &is_number_pair, "number pair");
  ly_add_type_predicate ((void *) &ly_is_list, "list");
  ly_add_type_predicate ((void *) &ly_is_procedure, "procedure");
  ly_add_type_predicate ((void *) &ly_is_symbol, "symbol");
  ly_add_type_predicate ((void *) &scm_is_bool, "boolean");
  ly_add_type_predicate ((void *) &scm_is_integer, "integer");
  ly_add_type_predicate ((void *) &scm_is_number, "number");
  ly_add_type_predicate ((void *) &scm_is_pair, "pair");
  ly_add_type_predicate ((void *) &scm_is_rational, "rational");
  ly_add_type_predicate ((void *) &scm_is_string, "string");
  ly_add_type_predicate ((void *) &unsmob<Transform>, "coordinate transform");
  ly_add_type_predicate ((void *) &scm_is_vector, "vector");
  ly_add_type_predicate ((void *) &unsmob<Item>, "Item");
  ly_add_type_predicate ((void *) &unsmob<Music>, "Music");
  ly_add_type_predicate ((void *) &unsmob<Spanner>, "Spanner");
  ly_add_type_predicate ((void *) &unsmob<Stream_event>, "Stream_event");
}

ADD_SCM_INIT_FUNC (func_doc, init_func_doc);
