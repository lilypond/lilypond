/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2023 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include <libguile.h>
#include <limits>
#include <string.h>

#include "axis.hh"
#include "memory.hh"
#include "interval.hh"
#include "lily-guile-macros.hh"

#include <functional>
#include <type_traits>
#include <utility>

class Bezier;
class Skyline;
class Skyline_pair;

/** Conversion functions follow the Guile naming convention, i.e.
    A ly_B2A (B b);  */

std::string ly_scm_write_string (SCM s);
SCM ly_deep_copy (SCM);

SCM ly_string2scm (std::string const &s);
std::string ly_scm2string (SCM s);
std::string ly_symbol2string (SCM);
std::string robust_symbol2string (SCM, const std::string &);
SCM ly_chain_assoc (SCM key, SCM achain);
SCM ly_chain_assoc_get (SCM key, SCM achain, SCM default_value,
                        SCM strict_checking = SCM_BOOL_F);

inline SCM
ly_assoc (SCM key, SCM alist)
{
  return (scm_is_symbol (key) || SCM_IMP (key)) ? scm_assq (key, alist)
                                                : scm_assoc (key, alist);
}

SCM ly_assoc_get (SCM key, SCM alist, SCM default_value,
                  SCM strict_checking = SCM_BOOL_F);
SCM ly_memv (SCM, SCM);
Slice int_list_to_slice (SCM l);
unique_stdlib_ptr<char> ly_scm2str0 (SCM str);

std::string robust_scm2string (SCM, const std::string &);

bool type_check_assignment (SCM val, SCM sym, SCM type_symbol);
std::string print_scm_val (SCM val);
SCM ly_number2string (SCM s);

SCM parse_symbol_list (char const *);
SCM robust_list_ref (int i, SCM l);

SCM ly_alist_vals (SCM alist);
SCM ly_hash2alist (SCM tab);
SCM ly_hash_table_keys (SCM tab);

SCM ly_assoc_prepend_x (SCM alist, SCM key, SCM val);
SCM ly_alist_copy (SCM alist);

inline bool
ly_is_list (SCM x)
{
  return scm_is_true (scm_list_p (x));
}
inline bool
ly_cheap_is_list (SCM x)
{
  return scm_is_pair (x) || scm_is_null (x);
}
inline bool
ly_is_module (SCM x)
{
  return SCM_MODULEP (x);
}
inline bool
ly_is_procedure (SCM x)
{
  return scm_is_true (scm_procedure_p (x));
}
inline bool
ly_is_port (SCM x)
{
  return scm_is_true (scm_port_p (x));
}

/*
  want to take the address of this function; scm_is_symbol() is a
  macro.
 */
inline bool
ly_is_symbol (SCM x)
{
  return scm_is_symbol (x);
}

inline bool
ly_is_equal (SCM x, SCM y)
{
  return scm_is_true (scm_equal_p (x, y));
}

inline bool
ly_is_eqv (SCM x, SCM y)
{
  return scm_is_true (scm_eqv_p (x, y));
}

/*
  display and print newline.
*/
extern "C"
{
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
void add_scm_init_func (void (*) ());

/*
  Inline these for performance reasons.
 */
#define scm_cdr ly_cdr
#define scm_car ly_car

inline SCM
ly_car (SCM x)
{
  return SCM_CAR (x);
}
inline SCM
ly_cdr (SCM x)
{
  return SCM_CDR (x);
}

// Wrap scm_call_... so that we don't have to count arguments.
template <typename... Args>
inline SCM
ly_call (SCM proc, Args &&...args)
{
  SCM argv[] = {std::forward<Args> (args)...};
  return scm_call_n (proc, argv, sizeof...(args));
}

// Same with scm_list_...  It's tempting to use scm_list_n (args..., SCM_UNDEFINED),
// but this doesn't work if any of args is SCM_UNDEFINED.  Recursion appears to be
// the most straightforward way to do this while not sacrificing type safety as
// would happen if forwarding the arguments to a function defined with C-style
// (va_list) variadic arguments.

inline SCM
ly_list ()
{
  return SCM_EOL;
}

template <typename... Args>
inline SCM
ly_list (SCM first, Args... args)
{
  return scm_cons (first, ly_list (args...));
}

// ly_append takes variadic arguments, unlike scm_append which takes an SCM list
// of arguments.
template <typename... Args>
inline SCM
ly_append (Args... args)
{
  return scm_append (ly_list (args...));
}

// Wrap scm_internal_hash_fold() to reduce the number of places we need to use
// reinterpret_cast.
inline SCM
ly_scm_hash_fold (SCM (*fn) (void *closure, SCM key, SCM val, SCM result),
                  void *closure, SCM init, SCM table)
{
  return scm_internal_hash_fold (reinterpret_cast<scm_t_hash_fold_fn> (fn),
                                 closure, init, table);
}

// C++ version of scm_with_fluid
SCM ly_with_fluid (SCM fluid, SCM val, std::function<SCM ()> const &);

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
  static SCM from (const SCM &&);

  static const SCM &to (const SCM &);
  static SCM &to (SCM &);
  static SCM to (const SCM &&);
};

// x_scm_t chooses the canonical type T for a to/from SCM conversion based on a
// function argument, e.g., `to_scm (value)`.  It allows an optional explicit
// type to override the decision; this supports (e.g.) `to_scm<T> (value)`,
// which may be needed to resolve ambiguity in some situations.
template <typename Deduced, typename Explicit = void>
struct x_scm_canonicalizer
{
  using type = std::remove_const_t<Explicit>;
};
template <typename Deduced>
struct x_scm_canonicalizer<Deduced>
{
  using type = std::remove_const_t<Deduced>;
};
template <typename Deduced, typename Explicit = void>
using x_scm_t = typename x_scm_canonicalizer<Deduced, Explicit>::type;

template <typename Deduced>
struct x_scm_canonicalizer<Deduced &> // remove deduced references
{
  using type = x_scm_t<Deduced>;
};

template <typename Deduced>
struct x_scm_canonicalizer<const Deduced *> // remove const but leave pointer
{
  using type = x_scm_t<Deduced *>;
};

// since partial template specialisation is not available for
// functions, we default to reflecting to a helper class for template
// types like Drul_array
template <typename T>
struct scm_conversions
{
};

// robust_scm_conversions<T> is a scm_conversions<T> with a robust overload of
// from_scm().  It should not be customized.  scm_conversions may be customized
// to provide a robust from_scm() itself, in which case robust_scm_conversions
// uses it rather than overriding it.
//
// This default robust_scm_conversions is used when the base scm_conversions
// does not have member functions.
template <typename T, typename = void>
struct robust_scm_conversions : public scm_conversions<T>
{
};

// This specialization of robust_scm_conversions is used when the base
// scm_conversions<T> has member functions.  For simplicity, we detect only
// is_scm() and expect other requirements to be met if that is present.
template <typename T>
class robust_scm_conversions<
  T, std::void_t<decltype (scm_conversions<T>::is_scm (SCM_EOL))>>
  : public scm_conversions<T>
{
private:
  using base_type = scm_conversions<T>;

  // is_base_robust<S, F>::value is true if the base provides from_scm(S, F).
  // C++20: Replace this detector with a `requires` on from_scm().
  template <typename S, typename F, typename = void>
  struct is_base_robust : public std::false_type
  {
  };
  template <typename S, typename F>
  struct is_base_robust<S, F,
                        std::void_t<decltype (base_type::from_scm (
                          std::declval<S> (), std::declval<F> ()))>>
    : public std::true_type
  {
  };

public:
  using base_type::from_scm;
  using base_type::is_scm;

  // If the base does not provide from_scm(S, F)...
  template <typename S, typename F,
            typename = std::enable_if_t<!is_base_robust<S, F>::value>>
  static decltype (auto) from_scm (S &&s, F &&fallback)
  {
    return base_type::is_scm (s) ? base_type::from_scm (std::forward<S> (s))
                                 : std::forward<F> (fallback);
  }
};

template <typename T>
inline bool
is_scm (SCM s)
{
  return scm_conversions<T>::is_scm (s);
}

template <typename ExplicitT, typename S,
          typename Conv = robust_scm_conversions<x_scm_t<void, ExplicitT>>>
inline auto
from_scm (S &&s) -> decltype (Conv::from_scm (std::forward<S> (s)))
{
  return Conv::from_scm (std::forward<S> (s));
}

// "robust" variant with fallback
template <typename ExplicitT = void, typename S, typename DeducedT,
          typename Conv = robust_scm_conversions<x_scm_t<DeducedT, ExplicitT>>>
inline auto
from_scm (S &&s, DeducedT &&fallback)
  -> decltype (Conv::from_scm (std::forward<S> (s),
                               std::forward<DeducedT> (fallback)))
{
  return Conv::from_scm (std::forward<S> (s),
                         std::forward<DeducedT> (fallback));
}

template <typename T>
inline auto
to_scm (const T &v) -> decltype (conv_scm_traits<T>::to (v))
{
  return scm_conversions<T>::to_scm (v);
}
template <typename T>
inline auto
to_scm (T &v) -> decltype (conv_scm_traits<T>::to (v))
{
  const auto &cv = v;
  return ::to_scm (cv); // defer to the const & overload
}
template <typename T>
inline auto
to_scm (const T &&v) -> decltype (conv_scm_traits<T>::to (std::move (v)))
{
  return ::to_scm (std::as_const (v)); // defer to the const & overload
}

// These pass-through conversions for SCM are useful in generic code.
template <>
struct scm_conversions<SCM>
{
  static bool is_scm (SCM) { return true; }

  static SCM &from_scm (SCM &s, SCM = SCM_UNDEFINED) { return s; }
  static const SCM &from_scm (const SCM &s, SCM = SCM_UNDEFINED) { return s; }
  static SCM from_scm (const SCM &&s, SCM = SCM_UNDEFINED) { return s; }
};

template <>
inline const SCM &
to_scm<SCM> (const SCM &s)
{
  return s;
}
template <>
inline SCM &
to_scm<SCM> (SCM &s)
{
  return s;
}

template <>
struct scm_conversions<short>
{
  static bool is_scm (SCM s)
  {
    using limits = std::numeric_limits<short>;
    return scm_is_signed_integer (s, limits::min (), limits::max ());
  }
  static short from_scm (SCM s) { return scm_to_short (s); }
  static SCM to_scm (const short &i) { return scm_from_short (i); }
};

template <>
struct scm_conversions<int>
{
  static bool is_scm (SCM s)
  {
    using limits = std::numeric_limits<int>;
    return scm_is_signed_integer (s, limits::min (), limits::max ());
  }
  static int from_scm (SCM s) { return scm_to_int (s); }
  static SCM to_scm (const int &i) { return scm_from_int (i); }
};

template <>
struct scm_conversions<long>
{
  static bool is_scm (SCM s)
  {
    using limits = std::numeric_limits<long>;
    return scm_is_signed_integer (s, limits::min (), limits::max ());
  }
  static long from_scm (SCM s) { return scm_to_long (s); }
  static SCM to_scm (const long &i) { return scm_from_long (i); }
};

template <>
struct scm_conversions<long long>
{
  static bool is_scm (SCM s)
  {
    using limits = std::numeric_limits<long long>;
    return scm_is_signed_integer (s, limits::min (), limits::max ());
  }
  static long long from_scm (SCM s) { return scm_to_long_long (s); }
  static SCM to_scm (const long long &i) { return scm_from_long_long (i); }
};

template <>
struct scm_conversions<unsigned short>
{
  static bool is_scm (SCM s)
  {
    using limits = std::numeric_limits<unsigned short>;
    return scm_is_unsigned_integer (s, limits::min (), limits::max ());
  }
  static unsigned short from_scm (SCM s) { return scm_to_ushort (s); }
  static SCM to_scm (const unsigned short &i) { return scm_from_ushort (i); }
};

template <>
struct scm_conversions<unsigned>
{
  static bool is_scm (SCM s)
  {
    using limits = std::numeric_limits<unsigned>;
    return scm_is_unsigned_integer (s, limits::min (), limits::max ());
  }
  static unsigned from_scm (SCM s) { return scm_to_uint (s); }
  static SCM to_scm (const unsigned &i) { return scm_from_uint (i); }
};

template <>
struct scm_conversions<unsigned long>
{
  static bool is_scm (SCM s)
  {
    using limits = std::numeric_limits<unsigned long>;
    return scm_is_unsigned_integer (s, limits::min (), limits::max ());
  }
  static unsigned long from_scm (SCM s) { return scm_to_ulong (s); }
  static SCM to_scm (const unsigned long &i) { return scm_from_ulong (i); }
};

template <>
struct scm_conversions<unsigned long long>
{
  static bool is_scm (SCM s)
  {
    using limits = std::numeric_limits<unsigned long long>;
    return scm_is_unsigned_integer (s, limits::min (), limits::max ());
  }
  static unsigned long long from_scm (SCM s) { return scm_to_ulong_long (s); }
  static SCM to_scm (unsigned long long i) { return scm_from_ulong_long (i); }
};

template <>
struct scm_conversions<bool>
{
  static bool is_scm (SCM s) { return scm_is_bool (s); }
  // from_scm implicitly falls back on false as that's what we generally need
  // for an undefined boolean.  This differs from Scheme which interprets
  // anything but #f as true.
  static bool from_scm (SCM s, bool fallback = false)
  {
    return fallback ? scm_is_true (s) : scm_is_eq (s, SCM_BOOL_T);
  }
  static SCM to_scm (bool i) { return scm_from_bool (i); }
};

template <>
struct scm_conversions<double>
{
  static bool is_scm (SCM s) { return scm_is_real (s); }
  static double from_scm (SCM s) { return scm_to_double (s); }
  static SCM to_scm (double i) { return scm_from_double (i); }
};

template <>
struct scm_conversions<Axis>
{
  static bool is_scm (SCM s)
  {
    return scm_is_unsigned_integer (s, X_AXIS, Y_AXIS);
  }
  static Axis from_scm (SCM s)
  {
    return Axis (scm_to_unsigned_integer (s, X_AXIS, Y_AXIS));
  }
  static SCM to_scm (Axis d) { return ::to_scm<int> (d); }
};

template <>
struct scm_conversions<Direction>
{
  static bool is_scm (SCM s) { return scm_is_signed_integer (s, LEFT, RIGHT); }
  // from_scm implicitly falls back on CENTER as that's what we generally need
  // for an undefined direction.
  static Direction from_scm (SCM s, Direction fallback = CENTER)
  {
    return is_scm (s) ? Direction (::from_scm<int> (s)) : fallback;
  }
  static SCM to_scm (Direction d) { return ::to_scm<int> (d); }
};

class Rational;

template <>
struct scm_conversions<Rational>
{
  static bool is_scm (SCM);
  static Rational from_scm (SCM);
  static SCM to_scm (const Rational &);
};

template <typename T>
inline bool
is_scm_pair (SCM s)
{
  return scm_is_pair (s) && is_scm<T> (scm_car (s)) && is_scm<T> (scm_cdr (s));
}
// No generic from_scm_pair and to_scm_pair for now since the
// construction and deconstruction of a pair-based type is not
// standardised well enough.  We could demand typename value_type,
// accessor functions first and second, and a two-argument
// constructor.  Not done for now.

class Offset;

template <>
struct scm_conversions<Offset>
{
  static bool is_scm (SCM s) { return is_scm_pair<Real> (s); }
  static Offset from_scm (SCM);
  static SCM to_scm (const Offset &);
};

template <>
struct scm_conversions<Bezier>
{
  static bool is_scm (SCM);
  static Bezier from_scm (SCM);
  static SCM to_scm (const Bezier &);
};

template <>
struct scm_conversions<Skyline_pair>
{
  static bool is_scm (SCM);
  static Skyline_pair from_scm (SCM);
  static SCM to_scm (const Skyline_pair &);
};

template <typename T>
struct scm_conversions<Drul_array<T>>
{
  static bool is_scm (SCM s)
  {
    return scm_is_pair (s) && ::is_scm<T> (scm_car (s))
           && ::is_scm<T> (scm_cdr (s));
  }
  static Drul_array<T> from_scm (SCM s)
  {
    return {::from_scm<T> (scm_car (s)), ::from_scm<T> (scm_cdr (s))};
  }
  static SCM to_scm (const Drul_array<T> &s)
  {
    return scm_cons (::to_scm (s[LEFT]), ::to_scm (s[RIGHT]));
  }
};

template <typename T>
struct scm_conversions<Interval_t<T>>
{
  static bool is_scm (SCM s)
  {
    return scm_is_pair (s) && ::is_scm<T> (scm_car (s))
           && ::is_scm<T> (scm_cdr (s));
  }
  static Interval_t<T> from_scm (SCM s)
  {
    return Interval_t<T> (::from_scm<T> (scm_car (s)),
                          ::from_scm<T> (scm_cdr (s)));
  }
  static SCM to_scm (const Interval_t<T> &s)
  {
    return scm_cons (::to_scm (s[LEFT]), ::to_scm (s[RIGHT]));
  }
};

// Convert the given SCM list to a container.
// The container must support the push_back method.
template <class T>
T
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
template <class T>
SCM
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
