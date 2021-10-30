/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2021 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include <limits>
#include <string.h>

/*
  Hack for various MacOS incarnations.
 */
#ifndef GUILE_ELLIPSIS
#define GUILE_ELLIPSIS
#endif

#include "axis.hh"
#include "memory.hh"
#include "guile-compatibility.hh"
#include "interval.hh"
#include "lily-guile-macros.hh"

/** Conversion functions follow the GUILE naming convention, i.e.
    A ly_B2A (B b);  */

SCM ly_last (SCM list);
std::string ly_scm_write_string (SCM s);
SCM ly_deep_copy (SCM);

std::string gulp_file_to_string (const std::string &fn, bool must_exist, int size);

SCM ly_string2scm (std::string const &s);
std::string ly_scm2string (SCM s);
std::string ly_symbol2string (SCM);
std::string robust_symbol2string (SCM, const std::string &);
SCM ly_chain_assoc (SCM key, SCM achain);
SCM ly_chain_assoc_get (SCM key, SCM achain, SCM default_value, SCM strict_checking = SCM_BOOL_F);

inline SCM ly_assoc (SCM key, SCM alist)
{
  return (scm_is_symbol (key) || SCM_IMP (key)) ? scm_assq (key, alist) : scm_assoc (key, alist);
}

SCM ly_assoc_get (SCM key, SCM alist, SCM default_value, SCM strict_checking = SCM_BOOL_F);
SCM ly_memv (SCM, SCM);
Slice int_list_to_slice (SCM l);
unique_stdlib_ptr<char> ly_scm2str0 (SCM str);

std::string robust_scm2string (SCM, const std::string &);

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

bool is_number_pair (SCM);

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

// Wrap scm_internal_hash_fold() to reduce the number of places we need to use
// reinterpret_cast.
inline SCM
ly_scm_hash_fold (SCM (*fn) (void *closure, SCM key, SCM val, SCM result),
                  void *closure, SCM init, SCM table)
{
#if !HAVE_GUILE_HASH_FUNC
  // For backward compatibility with Guile 1.8
  typedef SCM (*scm_t_hash_fold_fn) (GUILE_ELLIPSIS);
#endif

  return scm_internal_hash_fold (reinterpret_cast<scm_t_hash_fold_fn> (fn),
                                 closure, init, table);
}

// These are patterns for conversion functions.  We currently use them to
// predict the return types of overloaded functions before they are defined,
// but other things could be added here, if necessary.
template <typename T>
struct conv_scm_traits
{
  static T from (SCM);

  static SCM to (const T &);
};

// specialization for SCM passthrough, which is convenient in generic code
template <>
struct conv_scm_traits<SCM>
{
  static const SCM &from (const SCM &);
  static SCM &from (SCM &);

  static const SCM &to (const SCM &);
  static SCM &to (SCM &);
};

// since partial template specialisation is not available for
// functions, we default to reflecting to a helper class for template
// types like Drul_array
template <typename T> struct scm_conversions;

template <typename T> inline bool
is_scm (SCM s)
{
  return scm_conversions<T>::is_scm (s);
}

template <typename T> inline auto
from_scm (const SCM &s)->decltype (conv_scm_traits<T>::from (s))
{
  return scm_conversions<T>::from_scm (s);
}
template <typename T> inline auto
from_scm (SCM &s)->decltype (conv_scm_traits<T>::from (s))
{
  const auto &cs = s;
  return ::from_scm<T> (cs); // defer to the const & overload
}

// "robust" variant with fallback
template <typename T> inline auto
from_scm (const SCM &s, T fallback)->decltype (conv_scm_traits<T>::from (s))
{
  return scm_conversions<T>::from_scm (s, fallback);
}
template <typename T> inline auto
from_scm (SCM &s, T fallback)->decltype (conv_scm_traits<T>::from (s))
{
  const auto &cs = s;
  return ::from_scm<T> (cs, fallback); // defer to the const & overload
}

template <typename T> inline auto
to_scm (const T &v)->decltype (conv_scm_traits<T>::to (v))
{
  return scm_conversions<T>::to_scm (v);
}
template <typename T> inline auto
to_scm (T &v)->decltype (conv_scm_traits<T>::to (v))
{
  const auto &cv = v;
  return ::to_scm (cv); // defer to the const & overload
}

template <typename T> struct scm_conversions
{
  // Add a default rule implementing robust_scm2T
  //
  // For better or worse, whenever we are specialising
  // scm_conversions, we'll need to add this rule back in.
  //
  // An alternative would be to have a separate specialisation class
  // just for the fallback
  static T from_scm (SCM s, T fallback)
  {
    return ::is_scm<T> (s) ? ::from_scm<T> (s) : fallback;
  }
};

// These pass-through conversions for SCM are useful in generic code.
template <> inline bool is_scm<SCM> (SCM) { return true; }

template <> inline const SCM &from_scm<SCM> (const SCM &s) { return s; }
template <> inline SCM &from_scm<SCM> (SCM &s) { return s; }

template <> inline const SCM &from_scm<SCM> (const SCM &s, SCM) { return s; }
template <> inline SCM &from_scm<SCM> (SCM &s, SCM) { return s; }

template <> inline const SCM &to_scm<SCM> (const SCM &s) { return s; }
template <> inline SCM &to_scm<SCM> (SCM &s) { return s; }

template <> inline bool
is_scm<int> (SCM s)
{
  return scm_is_signed_integer (s,
                                std::numeric_limits<int>::min (),
                                std::numeric_limits<int>::max ());
}
template <> inline int
from_scm<int> (const SCM &s)
{
  return scm_to_int (s);
}
template <> inline SCM
to_scm<int> (const int &i)
{
  return scm_from_int (i);
}

template <> inline bool
is_scm<size_t> (SCM s)
{
  return scm_is_unsigned_integer (s,
                                  std::numeric_limits<size_t>::min (),
                                  std::numeric_limits<size_t>::max ());
}
template <> inline size_t
from_scm<size_t> (const SCM &s)
{
  return scm_to_size_t (s);
}
template <> inline SCM
to_scm<size_t> (const size_t &i)
{
  return scm_from_size_t (i);
}

// The following specializations for unsigned are declared inside
// scm_conversions<unsigned> because on 32 bit targets, unsigned
// and size_t are the  same type. Having multiple definitions is
// an error, but implementing an unused fallback is ok.
template <>
struct scm_conversions <unsigned>
{
  static bool is_scm (SCM s)
  {
    return scm_is_unsigned_integer (s,
                                    std::numeric_limits<unsigned>::min (),
                                    std::numeric_limits<unsigned>::max ());
  }
  static unsigned from_scm (SCM s) { return scm_to_uint (s); }
  static unsigned from_scm (SCM s, unsigned fallback)
  {
    return is_scm (s) ? from_scm (s) : fallback;
  }
  static SCM to_scm (unsigned i) { return scm_from_uint (i); }
};

template <> inline bool
is_scm<long> (SCM s)
{
  return scm_is_signed_integer (s,
                                std::numeric_limits<long>::min (),
                                std::numeric_limits<long>::max ());
}
template <> inline long
from_scm<long> (const SCM &s)
{
  return scm_to_long (s);
}
template <> inline SCM
to_scm<long> (const long &i)
{
  return scm_from_long (i);
}

template <> inline bool
is_scm<long long> (SCM s)
{
  return scm_is_signed_integer (s,
                                std::numeric_limits<long long>::min (),
                                std::numeric_limits<long long>::max ());
}
template <> inline long long
from_scm<long long> (const SCM &s)
{
  return scm_to_long_long (s);
}
template <> inline SCM
to_scm<long long> (const long long &i)
{
  return scm_from_long_long (i);
}

template <> inline bool
is_scm<bool> (SCM s)
{
  return scm_is_bool (s);
}
// from_scm<bool> does not error out for a non-boolean but defaults to
// #f as that's what we generally need for an undefined boolean.  This
// differs from Scheme which interprets anything but #f as true.
template <> inline bool
from_scm<bool> (const SCM &s)
{
  return scm_is_eq (s, SCM_BOOL_T);
}
template <> inline bool
from_scm<bool> (const SCM &s, bool fallback)
{
  if (fallback)
    return scm_is_true (s);
  else
    return from_scm<bool> (s);
}
template <> inline SCM
to_scm<bool> (const bool &i)
{
  return scm_from_bool (i);
}

template <> inline bool
is_scm<double> (SCM s)
{
  return scm_is_real (s);
}
template <> inline double
from_scm<double> (const SCM &s)
{
  return scm_to_double (s);
}
template <> inline SCM
to_scm<double> (const double &i)
{
  return scm_from_double (i);
}

template <> inline bool
is_scm<Axis> (SCM s)
{
  return scm_is_unsigned_integer (s, X_AXIS, Y_AXIS);
}
template <> inline Axis
from_scm<Axis> (const SCM &s)
{
  return Axis (scm_to_unsigned_integer (s, X_AXIS, Y_AXIS));
}
template <> inline SCM
to_scm<Axis> (const Axis &d)
{
  return to_scm<int> (d);
}

template <> inline bool
is_scm<Direction> (SCM s)
{
  return scm_is_signed_integer (s, LEFT, RIGHT);
}
// from_scm<Direction> does not error out for a non-direction but
// defaults to CENTER as that's what we generally need for an
// undefined direction.  In order not to have to call
// is_scm<Direction> more than once, we hard-code the defaulting
// variant and implement the one-argument version based on it.
template <> inline Direction
from_scm<Direction> (const SCM &s, Direction fallback)
{
  return is_scm<Direction> (s) ? Direction (scm_to_int (s)) : fallback;
}
template <> inline Direction
from_scm<Direction> (const SCM &s)
{
  return from_scm<Direction> (s, CENTER);
}
template <> inline SCM
to_scm<Direction> (const Direction &d)
{
  return to_scm<int> (d);
}

template <> bool is_scm<Rational> (SCM s);
template <> Rational from_scm<Rational> (const SCM &s);
template <> SCM to_scm<Rational> (const Rational &i);

template <typename T> inline bool
is_scm_pair (SCM s)
{
  return scm_is_pair (s) && is_scm<T> (scm_car (s)) && is_scm<T> (scm_cdr (s));
}
// No generic from_scm_pair and to_scm_pair for now since the
// construction and deconstruction of a pair-based type is not
// standardised well enough.  We could demand typename value_type,
// accessor functions first and second, and a two-argument
// constructor.  Not done for now.

template <> inline bool
is_scm<Offset> (SCM s)
{
  return is_scm_pair<Real> (s);
}

template <> Offset from_scm<Offset> (const SCM &s);
template <> SCM to_scm<Offset> (const Offset &i);

// partial function specialisation is not allowed, partially
// specialize helper class
template <typename T>
struct scm_conversions <T *>
{
  static bool is_scm (SCM s) { return unsmob<T> (s); }
  static T *from_scm (SCM s) { return unsmob<T> (s); }
  static T *from_scm (SCM s, T *fallback)
  {
    if (T *res = unsmob<T> (s))
      return res;
    return fallback;
  }
  static SCM to_scm (T *p) { return p->self_scm (); }
};

template <typename T>
struct scm_conversions <Drul_array<T>>
{
  static bool is_scm (SCM s)
  {
    return scm_is_pair (s) && ::is_scm<T> (scm_car (s)) && ::is_scm<T> (scm_cdr (s));
  }
  static Drul_array<T> from_scm (SCM s)
  {
    return Drul_array<T> (::from_scm<T> (scm_car (s)), ::from_scm<T> (scm_cdr (s)));
  }
  static Drul_array<T> from_scm (SCM s, Drul_array<T> fallback)
  {
    return is_scm (s) ? from_scm (s) : fallback;
  }
  static SCM to_scm (const Drul_array<T> &s)
  {
    return scm_cons (::to_scm (s[LEFT]), ::to_scm (s[RIGHT]));
  }
};

template <typename T>
struct scm_conversions <Interval_t<T>>
{
  static bool is_scm (SCM s)
  {
    return scm_is_pair (s) && ::is_scm<T> (scm_car (s)) && ::is_scm<T> (scm_cdr (s));
  }
  static Interval_t<T> from_scm (SCM s)
  {
    return Interval_t<T> (::from_scm<T> (scm_car (s)), ::from_scm<T> (scm_cdr (s)));
  }
  static Interval_t<T> from_scm (SCM s, Interval_t<T> fallback)
  {
    return is_scm (s) ? from_scm (s) : fallback;
  }
  static SCM to_scm (const Interval_t<T> &s)
  {
    return scm_cons (::to_scm (s[LEFT]), ::to_scm (s[RIGHT]));
  }
};

// Convert the given SCM list to a container.
// The container must support the push_back method.
template <class T> T
from_scm_list (SCM s)
{
  T ct;
  for (; scm_is_pair (s); s = scm_cdr (s))
    {
      ct.push_back (from_scm<typename T::value_type> (scm_car (s)));
    }
  return ct;
}
// Convert the given container to an SCM list.
// The container must support reverse iteration.
template <class T> SCM
to_scm_list (const T &ct)
{
  SCM lst = SCM_EOL;
  for (auto i = ct.crbegin (); i != ct.crend (); ++i)
    {
      lst = scm_cons (::to_scm (*i), lst);
    }
  return lst;
}

#endif /* LILY_GUILE_HH */
