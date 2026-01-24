/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <climits>
#include <cstdint>
#include <cstring>
#include <unordered_map>
#include <type_traits>

static Protected_scm doc_hash_table;

void
ly_check_name (const char *cxx, const char *scm_name)
{
  std::string mangle = mangle_cxx_identifier (cxx);
  if (mangle != scm_name)
    {
      programming_error ("wrong cxx name: " + mangle + ", " + cxx + ", "
                         + scm_name);
    }
}

void
ly_add_function_documentation (SCM func, const char *fname, const char *varlist,
                               const char *doc)
{
  // doc is a null-terminated character string, so check if it is empty.
  if (*doc == 0)
    return;

  if (!doc_hash_table.is_bound ())
    doc_hash_table = scm_c_make_hash_table (59);

  std::string s = std::string (" - ") + "LilyPond procedure: " + fname + " "
                  + varlist + "\n" + doc;

  scm_set_procedure_property_x (func, ly_symbol2scm ("documentation"),
                                to_scm (s));
  SCM entry = scm_cons (to_scm (varlist), to_scm (doc));
  scm_hashq_set_x (doc_hash_table, ly_symbol2scm (fname), entry);
}

LY_DEFINE (ly_get_all_function_documentation,
           "ly:get-all-function-documentation", 0, 0, 0, (),
           R"(
Get a hash table with all LilyPond Scheme extension functions.
           )")
{
  return doc_hash_table;
}

// Portably get a description for integer predicates that include range checks.
namespace
{

template <typename SIGN_TAG, std::size_t BITS>
struct integral_descriptor;

template <>
struct integral_descriptor<signed, 16>
{
  static constexpr auto text = "integer in [-2^15, 2^15)";
};

template <>
struct integral_descriptor<signed, 32>
{
  static constexpr auto text = "integer in [-2^31, 2^31)";
};

template <>
struct integral_descriptor<signed, 64>
{
  static constexpr auto text = "integer in [-2^63, 2^63)";
};

template <>
struct integral_descriptor<unsigned, 16>
{
  static constexpr auto text = "integer in [0, 2^16)";
};

template <>
struct integral_descriptor<unsigned, 32>
{
  static constexpr auto text = "integer in [0, 2^32)";
};

template <>
struct integral_descriptor<unsigned, 64>
{
  static constexpr auto text = "integer in [0, 2^64)";
};

template <typename T>
constexpr const char *
int_text (T = {})
{
  using namespace std;
  using s = typename conditional<is_signed<T>::value, signed, unsigned>::type;
  return integral_descriptor<s, CHAR_BIT * sizeof (T)>::text;
}

} // namespace

void
init_func_doc ()
{
  // C++ integral types
  ly_predicate_info<is_scm<short>>::init (int_text<short> ());
  ly_predicate_info<is_scm<int>>::init (int_text (0));
  ly_predicate_info<is_scm<long>>::init (int_text (0L));
  ly_predicate_info<is_scm<long long>>::init (int_text (0LL));
  ly_predicate_info<is_scm<unsigned short>>::init (int_text<unsigned short> ());
  ly_predicate_info<is_scm<unsigned>>::init (int_text (0U));
  ly_predicate_info<is_scm<unsigned long>>::init (int_text (0UL));
  ly_predicate_info<is_scm<unsigned long long>>::init (int_text (0ULL));

  // etc.
  ly_predicate_info<is_number_pair>::init ("number pair");
  ly_predicate_info<is_scm<Axis>>::init ("axis");
  ly_predicate_info<is_scm<Direction>>::init ("direction");
  ly_predicate_info<is_scm<Offset>>::init ("pair of reals");
  ly_predicate_info<is_scm<Bezier>>::init ("list of four number pairs");
  ly_predicate_info<is_scm<Skyline_pair>>::init ("pair of skylines");
  ly_predicate_info<ly_cheap_is_list>::init ("list");
  ly_predicate_info<ly_is_list>::init ("list");
  ly_predicate_info<ly_is_port>::init ("port");
  ly_predicate_info<ly_is_procedure>::init ("procedure");
  ly_predicate_info<ly_is_symbol>::init ("symbol");
  ly_predicate_info<scm_is_bool>::init ("boolean");
  ly_predicate_info<scm_is_bytevector>::init ("bytevector");
  ly_predicate_info<scm_is_integer>::init ("integer");
  ly_predicate_info<scm_is_number>::init ("number");
  ly_predicate_info<scm_is_pair>::init ("pair");
  ly_predicate_info<is_scm<Rational>>::init ("rational");
  ly_predicate_info<scm_is_real>::init ("real number");
  ly_predicate_info<scm_is_string>::init ("string");
  ly_predicate_info<scm_is_vector>::init ("vector");
}

ADD_SCM_INIT_FUNC (func_doc, init_func_doc);
