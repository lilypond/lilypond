/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef LILY_GUILE_HH
#define LILY_GUILE_HH

#include "config.hh"

#if __MINGW32__
#include "mingw-compatibility.hh"
#endif

#if HAVE_LIBGUILE18_H
# include <libguile18.h>
#else
# include <libguile.h>
#endif
#include <string.h>

/*
  Hack for various MacOS incarnations.
 */
#ifndef GUILE_ELLIPSIS
#define GUILE_ELLIPSIS
#endif

#include "guile-compatibility.hh"
#include "interval.hh"
#include "lily-guile-macros.hh"
#include "std-vector.hh"

/** Conversion functions follow the GUILE naming convention, i.e.
    A ly_B2A (B b);  */

SCM ly_last (SCM list);
std::string ly_scm_write_string (SCM s);
SCM ly_deep_copy (SCM);
SCM ly_truncate_list (int k, SCM lst);

std::string gulp_file_to_string (const std::string &fn, bool must_exist, int size);

SCM ly_string2scm (std::string const &s);
std::string ly_scm2string (SCM s);
std::string ly_symbol2string (SCM);
std::string robust_symbol2string (SCM, const std::string &);
Rational ly_scm2rational (SCM);
SCM ly_rational2scm (Rational);
SCM ly_offset2scm (Offset);
Offset ly_scm2offset (SCM);
SCM ly_chain_assoc (SCM key, SCM achain);
SCM ly_chain_assoc_get (SCM key, SCM achain, SCM default_value, SCM strict_checking = SCM_BOOL_F);

inline SCM ly_assoc (SCM key, SCM alist)
{
  return (scm_is_symbol (key) || SCM_IMP (key)) ? scm_assq (key, alist) : scm_assoc (key, alist);
}

SCM ly_assoc_get (SCM key, SCM alist, SCM default_value, SCM strict_checking = SCM_BOOL_F);
Interval ly_scm2interval (SCM);
Drul_array<Real> ly_scm2realdrul (SCM);
SCM ly_memv (SCM, SCM);
Slice int_list_to_slice (SCM l);
SCM ly_interval2scm (Drul_array<Real>);
char *ly_scm2str0 (SCM str);

Real robust_scm2double (SCM, double);
int robust_scm2int (SCM, int);
vsize robust_scm2vsize (SCM, vsize);
Direction robust_scm2dir (SCM, Direction);
Drul_array<Real> robust_scm2drul (SCM, Drul_array<Real>);
Drul_array<bool> robust_scm2booldrul (SCM, Drul_array<bool>);
Interval robust_scm2interval (SCM, Drul_array<Real>);
Offset robust_scm2offset (SCM, Offset);
std::string robust_scm2string (SCM, const std::string &);
Rational robust_scm2rational (SCM, Rational);
std::vector<Real> ly_scm2floatvector (SCM);
SCM ly_floatvector2scm (std::vector<Real> v);

SCM ly_quote_scm (SCM s);
bool type_check_assignment (SCM val, SCM sym, SCM type_symbol);
std::string print_scm_val (SCM val);
SCM ly_number2string (SCM s);

SCM parse_symbol_list (char const *);
SCM robust_list_ref (int i, SCM l);
SCM alist_to_hashq (SCM);

SCM ly_alist_vals (SCM alist);
SCM ly_hash2alist (SCM tab);
SCM ly_hash_table_keys (SCM tab);

SCM ly_assoc_prepend_x (SCM alist, SCM key, SCM val);
SCM ly_alist_copy (SCM alist);

inline bool ly_is_list (SCM x) { return scm_is_true (scm_list_p (x)); }
inline bool ly_cheap_is_list (SCM x) { return scm_is_pair (x) || scm_is_null (x); }
inline bool ly_is_module (SCM x) { return SCM_MODULEP (x); }
inline bool ly_is_procedure (SCM x) { return scm_is_true (scm_procedure_p (x)); }
inline bool ly_is_port (SCM x) { return scm_is_true (scm_port_p (x)); }

bool ly_is_rational (SCM);
/*
  want to take the address of this function; scm_is_symbol() is a
  macro.
 */
inline bool ly_is_symbol (SCM x) { return scm_is_symbol (x); }

inline bool ly_is_equal (SCM x, SCM y)
{
  return scm_is_true (scm_equal_p (x, y));
}

inline bool ly_scm2bool (SCM x) { return scm_is_true (x); }
inline char ly_scm2char (SCM x) { return (char)SCM_CHAR (x); }
inline SCM ly_bool2scm (bool x) { return scm_from_bool (x); }

inline SCM ly_append2 (SCM x1, SCM x2)
{
  return scm_append (scm_list_2 (x1, x2));
}
inline SCM ly_append3 (SCM x1, SCM x2, SCM x3)
{
  return scm_append (scm_list_3 (x1, x2, x3));
}
inline SCM ly_append4 (SCM x1, SCM x2, SCM x3, SCM x4)
{
  return scm_append (scm_list_4 (x1, x2, x3, x4));
}

/*
  display and print newline.
*/
extern "C" {
  void ly_display_scm (SCM s);
}

void read_lily_scm_file (std::string);
void ly_c_init_guile ();

bool is_direction (SCM s);
bool is_number_pair (SCM);
bool is_axis (SCM);

/*
  these conversion functions also do a typecheck on the argument, and
  return a default value if S has the wrong type.
*/

Direction to_dir (SCM s);
bool to_boolean (SCM s);

SCM index_get_cell (SCM cell, Direction d);
SCM index_set_cell (SCM cell, Direction d, SCM val);

/*
  snarfing.
*/
void add_scm_init_func (void ( *) ());

/*
  Inline these for performance reasons.
 */
#define scm_cdr ly_cdr
#define scm_car ly_car

#ifndef scm_is_pair
#define scm_is_pair ly_is_pair
#endif

inline SCM ly_car (SCM x) { return SCM_CAR (x); }
inline SCM ly_cdr (SCM x) { return SCM_CDR (x); }
inline bool ly_is_pair (SCM x) { return SCM_I_CONSP (x); }

template<class T>
SCM
ly_cxx_vector_to_list (std::vector<T> const &src)
{
  SCM l = SCM_EOL;
  for (vsize i = src.size (); i--;)
    l = scm_cons (src[i]->self_scm (), l);

  return l;
}

SCM ly_offsets2scm (std::vector<Offset> os);
std::vector<Offset> ly_scm2offsets (SCM s);

/* For backward compatability with Guile 1.8 */
#if !HAVE_GUILE_HASH_FUNC
typedef SCM (*scm_t_hash_fold_fn) (GUILE_ELLIPSIS);
#endif

#endif /* LILY_GUILE_HH */
