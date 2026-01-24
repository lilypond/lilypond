/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef LILY_GUILE_MACROS_HH
#define LILY_GUILE_MACROS_HH

#include "config.hh"

#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

/* this lets us "overload" macros such as get_property to take
   symbols as well as strings */
inline SCM
scm_or_str2symbol (char const *c)
{
  return scm_from_utf8_symbol (c);
}

inline SCM
scm_or_str2symbol (const std::string &s)
{
  return scm_from_utf8_symbol (s.c_str ());
}

inline SCM
scm_or_str2symbol (SCM s)
{
  assert (scm_is_symbol (s));
  return s;
}

/* Same, with keywords */
inline SCM
scm_or_str2keyword (char const *c)
{
  return scm_from_utf8_keyword (c);
}

inline SCM
scm_or_str2keyword (const std::string &s)
{
  return scm_from_utf8_keyword (s.c_str ());
}

inline SCM
scm_or_str2keyword (SCM s)
{
  assert (scm_is_keyword (s));
  return s;
}

/* Using this trick we cache the value of scm_from_utf8_{symbol,keyword} ("fooo")
   where "fooo" is a constant char *. This is done at the cost of one
   static variable per ly_{symbol,keyword}2scm() use, and the cost of C++'s
   mechanism to ensure a static variable is only initialized once.
 */
#define ly_internal_symbol_or_keyword2scm(converter, x)                        \
  (__builtin_constant_p (x)                                             \
   ? [&] {                                                              \
     static SCM cached = scm_gc_protect_object (converter (x));         \
     return cached;                                                     \
   } ()                                                                 \
   : converter (x))

#define ly_symbol2scm(x)                                                       \
  ly_internal_symbol_or_keyword2scm (scm_or_str2symbol, x)
#define ly_keyword2scm(x)                                                      \
  ly_internal_symbol_or_keyword2scm (scm_or_str2keyword, x)

/*
  Adds the NAME as a Scheme function, and a variable to store the SCM
  version of the function in the static variable NAME_proc
*/
#define DECLARE_SCHEME_CALLBACK(NAME, ARGS)                                    \
  static SCM NAME ARGS;                                                        \
  static SCM NAME##_proc

std::string mangle_cxx_identifier (const char *);

// Map a predicate to a user-facing type name at compile time.  A pointer to the
// name is known at compile time, but the name itself is filled in at run time.
// C++20: Try setting the name at compile time.
template <auto pred>
class ly_predicate_info
{
  static_assert (decltype (static_cast<bool> (pred (SCM_EOL))) {true},
                 "predicate return type must be convertible to bool");

private:
  static inline std::unique_ptr<char[]> name_;

public:
  static void init (std::string_view name)
  {
    auto new_name = std::unique_ptr<char[]> (new char[name.size ()]);
    std::uninitialized_copy (name.begin (), name.end (), new_name.get ());
    name_ = std::move (new_name);
  }
  static const char *name () { return name_ ? name_.get () : "unknown type"; }
};

namespace detail
{
// ly_scm_func_of_arity<n>::ptr_type is a pointer to a function taking n SCM
// arguments and returning SCM.  This is a helper for ly_subr_ptr, which adds
// some distinctions and restrictions.
template <unsigned N, typename... Args>
struct ly_scm_func_of_arity
{
  // recur, adding one SCM to the parameter pack at each level
  using type = typename ly_scm_func_of_arity<N - 1, SCM, Args...>::type;
};

template <typename... Args>
struct ly_scm_func_of_arity<0, Args...>
{
  // at bottom, declare the type using the accumulated SCM parameters
  using type = SCM (*) (Args...);
};
} // namespace detail

// This type statically associates a set of argument counts with the type of a
// raw function having the specified total number of SCM arguments and returning
// SCM.  Creating an instance of ly_subr_ptr from an overloaded function pointer
// automatically resolves using the expected signature (or fails if there is no
// matching overload).
template <unsigned N_REQ, unsigned N_OPT = 0, unsigned N_REST = 0>
struct ly_subr_ptr
{
public:
  static inline constexpr unsigned num_required_args = N_REQ;
  static inline constexpr unsigned num_optional_args = N_OPT;
  static inline constexpr unsigned num_rest_args = N_REST;
  static inline constexpr unsigned num_args_total
    = num_required_args + num_optional_args + num_rest_args;

  static_assert (num_rest_args <= 1, "as doc'd for scm_c_make_gsubr ()");
  static_assert (num_args_total <= 10, "as doc'd for scm_c_make_gsubr ()");

  using raw_ptr = typename detail::ly_scm_func_of_arity<num_args_total>::type;

public: // to keep it literal, anticipating use as C++20 non-type template arg
  raw_ptr private_func_;

public:
  // If there were a need to get the raw pointer, get() would be the way.
  // If there were a need to invoke the function, operator() would be the way.

  // allow static_cast<scm_t_subr>
  explicit operator scm_t_subr () const
  {
    return reinterpret_cast<scm_t_subr> (private_func_); // C++20: std::bit_cast
  };
};

/*
  Two main macros are provided in order to export C++ functions,
  making them accessible to Scheme: LY_DEFINE and MAKE_SCHEME_CALLBACK.
  LY_DEFINE is used like this:

    LY_DEFINE (ly_cxx_name, "ly:scheme-name",
               req, opt, var,
               (SCM arg1, SCM arg2, [...]),
               R"(
    The docstring.
               )")
    {
      [... function body, returning SCM ...]
    }

  For grob callbacks, you'd use:

    // declaration
    class The_interface
    {
      DECLARE_SCHEME_CALLBACK (cxx_name, (SCM smob, SCM other_arg1, ...);
      [...]
    };

    // definition
    MAKE_SCHEME_CALLBACK (The_interface, cxx_name,
                          "ly:the-interface::scheme-name", arg_count);
    SCM
    The_interface::cxx_name (SCM smob, SCM other_arg1, [...])
    {
      [function body, returning SCM ...]
    }

  The req, opt and var arguments of LY_DEFINE are argument counts, as
  specified in the Guile documentation (node "Primitive procedures").
  For MAKE_SCHEME_CALLBACK, the number of arguments is always
  constant.

  Docstrings should be written as C++11 raw strings for the sake of
  readability and editability.
*/

#define MAKE_SCHEME_CALLBACK_WITH_OPTARGS(TYPE, FUNC, PRIMNAME, ARGCOUNT,      \
                                          OPTIONAL_COUNT, DOC)                 \
  SCM TYPE ::FUNC##_proc;                                                      \
  void TYPE##_##FUNC##_init_functions ()                                       \
  {                                                                            \
    constexpr auto required_count = ARGCOUNT - OPTIONAL_COUNT;                 \
    constexpr auto func                                                        \
      = ly_subr_ptr<required_count, OPTIONAL_COUNT> {TYPE::FUNC};              \
    TYPE ::FUNC##_proc                                                         \
      = scm_c_define_gsubr (PRIMNAME, required_count, OPTIONAL_COUNT, 0,       \
                            static_cast<scm_t_subr> (func));                   \
    ly_check_name (#TYPE "::" #FUNC, PRIMNAME);                                \
    ly_add_function_documentation (TYPE ::FUNC##_proc, PRIMNAME, "", DOC);     \
    scm_c_export (PRIMNAME, NULL);                                             \
  }                                                                            \
                                                                               \
  ADD_SCM_INIT_FUNC (TYPE##_##FUNC##_callback, TYPE##_##FUNC##_init_functions);

#define MAKE_DOCUMENTED_SCHEME_CALLBACK(TYPE, FUNC, PRIMNAME, ARGCOUNT, DOC)   \
  MAKE_SCHEME_CALLBACK_WITH_OPTARGS (TYPE, FUNC, PRIMNAME, ARGCOUNT, 0, DOC);

#define MAKE_SCHEME_CALLBACK(TYPE, FUNC, PRIMNAME, ARGCOUNT)                   \
  MAKE_SCHEME_CALLBACK_WITH_OPTARGS (TYPE, FUNC, PRIMNAME, ARGCOUNT, 0, "");

void ly_add_function_documentation (SCM proc, const char *fname,
                                    const char *varlist, const char *doc);
void ly_check_name (const char *cxx, const char *fname);

#define ADD_SCM_INIT_FUNC(name, func)                                          \
  auto ly_##name##_scm_initter = (add_scm_init_func (func), std::monostate {})

/* end define */

#define LY_DEFINE_WITHOUT_DECL(INITPREFIX, FNAME, PRIMNAME, REQ, OPT, VAR,     \
                               ARGLIST, DOCSTRING)                             \
  SCM FNAME##_proc;                                                            \
  void INITPREFIX##init ()                                                     \
  {                                                                            \
    constexpr auto func = ly_subr_ptr<REQ, OPT, VAR> {FNAME};                  \
    FNAME##_proc = scm_c_define_gsubr (PRIMNAME, REQ, OPT, VAR,                \
                                       static_cast<scm_t_subr> (func));        \
    ly_check_name (#FNAME, PRIMNAME);                                          \
    ly_add_function_documentation (FNAME##_proc, PRIMNAME, #ARGLIST,           \
                                   DOCSTRING);                                 \
    scm_c_export (PRIMNAME, NULL);                                             \
  }                                                                            \
  ADD_SCM_INIT_FUNC (INITPREFIX##init_unique_prefix, INITPREFIX##init);        \
  SCM FNAME ARGLIST

#define LY_DEFINE(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING)          \
  SCM FNAME ARGLIST;                                                           \
  LY_DEFINE_WITHOUT_DECL (FNAME, FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST,      \
                          DOCSTRING)

#define LY_DEFINE_MEMBER_FUNCTION(CLASS, FNAME, PRIMNAME, REQ, OPT, VAR,       \
                                  ARGLIST, DOCSTRING)                          \
  LY_DEFINE_WITHOUT_DECL (CLASS##FNAME, CLASS::FNAME, PRIMNAME, REQ, OPT, VAR, \
                          ARGLIST, DOCSTRING)

/* LY_DEFINE_WITH_SETTER is like LY_DEFINE, but adding a setter procedure.
   Adding, for example, ly:music-set-property! as setter to ly:music-property
   means one can write
     (set! (ly:music-property m 'property) ...)
   equivalently to
     (ly:music-set-property! m 'property ...)
   For this to work, the setter must be defined before the getter, and
   within the same compilation unit (so the init function adding it is
   guaranteed to be defined first). */
#define LY_DEFINE_WITH_SETTER(FNAME, PRIMNAME, SETTERNAME, REQ, OPT, VAR,      \
                              ARGLIST, DOCSTRING)                              \
  SCM FNAME ARGLIST;                                                           \
  SCM FNAME##_proc_without_setter;                                             \
  SCM FNAME##_proc;                                                            \
  void FNAME##init ()                                                          \
  {                                                                            \
    constexpr auto func = ly_subr_ptr<REQ, OPT, VAR> {FNAME};                  \
    FNAME##_proc_without_setter = scm_c_make_gsubr (                           \
      PRIMNAME, REQ, OPT, VAR, static_cast<scm_t_subr> (func));                \
    ly_check_name (#FNAME, PRIMNAME);                                          \
    FNAME##_proc = scm_make_procedure_with_setter (                            \
      FNAME##_proc_without_setter, SETTERNAME##_proc);                         \
    ly_add_function_documentation (FNAME##_proc, PRIMNAME, #ARGLIST,           \
                                   DOCSTRING);                                 \
    scm_c_define (PRIMNAME, FNAME##_proc);                                     \
    scm_c_export (PRIMNAME, NULL);                                             \
  }                                                                            \
  ADD_SCM_INIT_FUNC (FNAME##init_unique_prefix, FNAME##init);                  \
  SCM FNAME ARGLIST

#define get_property(p, x) (p)->internal_get_property (ly_symbol2scm (x))
#define get_pure_property(p, x, y, z)                                          \
  (p)->internal_get_pure_property (ly_symbol2scm (x), y, z)
#define get_maybe_pure_property(p, w, x, y, z)                                 \
  (p)->internal_get_maybe_pure_property (ly_symbol2scm (w), x, y, z)
#define get_property_data(p, x)                                                \
  (p)->internal_get_property_data (ly_symbol2scm (x))
#define get_object(p, x) (p)->internal_get_object (ly_symbol2scm (x))
#define set_object(p, x, y) (p)->internal_set_object (ly_symbol2scm (x), y)
#define del_property(p, x) (p)->internal_del_property (ly_symbol2scm (x))

// Is the variable named x defined at p?  Does not check enclosing scopes.  An
// optional parameter receives the value if defined.  Returns bool.
#define here_defined(p, x, ...)                                                \
  (p)->internal_here_defined (ly_symbol2scm (x), ##__VA_ARGS__)

// Where is the variable named x defined?  Checks enclosing scopes starting
// from p.  An optional parameter receives the value if defined.  Returns
// pointer.
#define where_defined(p, x, ...)                                               \
  (p)->internal_where_defined (ly_symbol2scm (x), ##__VA_ARGS__)

/*
  TODO: include modification callback support here, perhaps
  through intermediate Grob::instrumented_set_property( .. __LINE__ ).
 */
#define set_property(p, x, y)                                                  \
  [&] {                                                                        \
    if constexpr (CHECKING)                                                    \
      {                                                                        \
        return (p)->instrumented_set_property (ly_symbol2scm (x), y, __FILE__, \
                                               __LINE__, __FUNCTION__);        \
      }                                                                        \
    else                                                                       \
      {                                                                        \
        return (p)->internal_set_property (ly_symbol2scm (x), y);              \
      }                                                                        \
  }()

// Note: For Smobs, use LY_ASSERT_SMOB instead.
#define LY_ASSERT_TYPE(pred, var, number)                                      \
  {                                                                            \
    if (!pred (var))                                                           \
      {                                                                        \
        scm_wrong_type_arg_msg (mangle_cxx_identifier (__FUNCTION__).c_str (), \
                                number, var,                                   \
                                ly_predicate_info<pred>::name ());             \
      }                                                                        \
  }

#endif /* LILY_GUILE_MACROS_HH */
