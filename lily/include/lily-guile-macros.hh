/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#if GUILEV2
// if Guile's internal representation switches to utf8, this should be
// changed accordingly for efficiency's sake.  This is used for
// strings known to be in ASCII entirely, including any std::string
// constants in the C code.
#define scm_from_ascii_string scm_from_latin1_string
#define scm_from_ascii_stringn scm_from_latin1_stringn
#define scm_from_ascii_symbol scm_from_latin1_symbol
#else
#define scm_from_ascii_string scm_from_locale_string
#define scm_from_ascii_stringn scm_from_locale_stringn
#define scm_from_ascii_symbol scm_from_locale_symbol
#define scm_from_latin1_string scm_from_locale_string
#define scm_from_latin1_stringn scm_from_locale_stringn
#define scm_from_utf8_string scm_from_locale_string
#define scm_from_utf8_stringn scm_from_locale_stringn
#define scm_from_utf8_symbol scm_from_locale_symbol
#define scm_to_utf8_string scm_to_locale_string
#define scm_to_utf8_stringn scm_to_locale_stringn
#endif

#ifndef SMOB_FREE_RETURN_VAL
#define SMOB_FREE_RETURN_VAL(CL) 0
#endif

/* For backward compatibility with Guile 1.8 */
#if !HAVE_GUILE_SUBR_TYPE
typedef SCM (*scm_t_subr) (GUILE_ELLIPSIS);
#endif

/* this lets us "overload" macros such as get_property to take
   symbols as well as strings */
inline SCM
scm_or_str2symbol (char const *c) { return scm_from_utf8_symbol (c); }

inline SCM
scm_or_str2symbol (SCM s)
{
  assert (scm_is_symbol (s));
  return s;
}

/* Using this trick we cache the value of scm_from_locale_symbol
   ("fooo") where "fooo" is a constant std::string. This is done at the
   cost of one static variable per ly_symbol2scm() use, and the cost
   of C++' mechanism to ensure a static variable is only initialized
   once.
 */
#define ly_symbol2scm(x)                                                \
  (__builtin_constant_p (x)                                             \
   ? ({                                                                 \
       static SCM cached = scm_gc_protect_object (scm_or_str2symbol (x)); \
       cached;                                                          \
     })                                                                 \
   : scm_or_str2symbol (x))

/*
  Adds the NAME as a Scheme function, and a variable to store the SCM
  version of the function in the static variable NAME_proc
*/
#define DECLARE_SCHEME_CALLBACK(NAME, ARGS)     \
  static SCM NAME ARGS;                         \
  static SCM NAME ## _proc

std::string mangle_cxx_identifier (const char *);

void ly_add_type_predicate (void *ptr, const char *name);
std::string predicate_to_typename (void *ptr);

// ly_scm_func_of_arity<n>::ptr_type is a pointer to a function taking n SCM
// arguments and returning SCM.
template <unsigned>
struct ly_scm_func_of_arity
{
  // ptr_type is defined in specializations
};

template <>
struct ly_scm_func_of_arity<0>
{
  typedef SCM (*ptr_type) ();
};

template <>
struct ly_scm_func_of_arity<1>
{
  typedef SCM (*ptr_type) (SCM);
};

template <>
struct ly_scm_func_of_arity<2>
{
  typedef SCM (*ptr_type) (SCM, SCM);
};

template <>
struct ly_scm_func_of_arity<3>
{
  typedef SCM (*ptr_type) (SCM, SCM, SCM);
};

template <>
struct ly_scm_func_of_arity<4>
{
  typedef SCM (*ptr_type) (SCM, SCM, SCM, SCM);
};

/*
  Make TYPE::FUNC available as a Scheme function.
*/
#define MAKE_SCHEME_CALLBACK_WITH_OPTARGS(TYPE, FUNC, ARGCOUNT, OPTIONAL_COUNT, DOC) \
  SCM TYPE ::FUNC ## _proc;                                             \
  void                                                                  \
  TYPE ## _ ## FUNC ## _init_functions ()                               \
  {                                                                     \
    std::string id = mangle_cxx_identifier (#TYPE "::" #FUNC);          \
    /* assignment selects the SCM function even if it is overloaded */ \
    ly_scm_func_of_arity<ARGCOUNT>::ptr_type func = TYPE::FUNC; \
    TYPE ::FUNC ## _proc = scm_c_define_gsubr (id.c_str(),                      \
                                               (ARGCOUNT-OPTIONAL_COUNT), OPTIONAL_COUNT, 0,    \
                                               (scm_t_subr) func); \
    ly_add_function_documentation (TYPE :: FUNC ## _proc, id.c_str(), "", \
                                   DOC);                                \
    scm_c_export (id.c_str (), NULL);                                   \
  }                                                                     \
                                                                        \
  ADD_SCM_INIT_FUNC (TYPE ## _ ## FUNC ## _callback,                    \
                     TYPE ## _ ## FUNC ## _init_functions);

#define MAKE_DOCUMENTED_SCHEME_CALLBACK(TYPE, FUNC, ARGCOUNT, DOC)              \
  MAKE_SCHEME_CALLBACK_WITH_OPTARGS(TYPE, FUNC, ARGCOUNT, 0, DOC);

#define MAKE_SCHEME_CALLBACK(TYPE, FUNC, ARGCOUNT)                      \
  MAKE_SCHEME_CALLBACK_WITH_OPTARGS(TYPE,FUNC,ARGCOUNT, 0, "");

void ly_add_function_documentation (SCM proc, const char *fname, const char *varlist, const char *doc);
void ly_check_name (const char *cxx, const char *fname);

#define ADD_SCM_INIT_FUNC(name, func)           \
  class name ## _scm_initter                    \
  {                                             \
  public:                                       \
    name ## _scm_initter ()                     \
    {                                           \
      add_scm_init_func (func);                 \
    }                                           \
  }                                             \
    _ ## name ## _scm_initter;

/* end define */

#define LY_DEFINE_WITHOUT_DECL(INITPREFIX, FNAME, PRIMNAME, REQ, OPT, VAR, \
                               ARGLIST, DOCSTRING)                      \
  SCM FNAME ## _proc;                                                   \
  void                                                                  \
  INITPREFIX ## init ()                                                 \
  {                                                                     \
    FNAME ## _proc = scm_c_define_gsubr (PRIMNAME, REQ, OPT, VAR,       \
                                         (scm_t_subr) FNAME); \
    ly_check_name (#FNAME, PRIMNAME);\
    ly_add_function_documentation (FNAME ## _proc, PRIMNAME, #ARGLIST,  \
                                   DOCSTRING);                          \
    scm_c_export (PRIMNAME, NULL);                                      \
  }                                                                     \
  ADD_SCM_INIT_FUNC (INITPREFIX ## init_unique_prefix, INITPREFIX ## init); \
  SCM                                                                   \
  FNAME ARGLIST

#define LY_DEFINE(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING)   \
  SCM FNAME ARGLIST;                                                    \
  LY_DEFINE_WITHOUT_DECL (FNAME, FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, \
                          DOCSTRING)

#define LY_DEFINE_MEMBER_FUNCTION(CLASS, FNAME, PRIMNAME, REQ, OPT, VAR, \
                                  ARGLIST, DOCSTRING)                   \
  LY_DEFINE_WITHOUT_DECL (CLASS ## FNAME, CLASS::FNAME, PRIMNAME, REQ, OPT, \
                          VAR, ARGLIST, DOCSTRING)

#define get_property(p,x) (p)->internal_get_property (ly_symbol2scm (x))
#define get_pure_property(p,x,y,z)                      \
  (p)->internal_get_pure_property (ly_symbol2scm (x), y, z)
#define get_maybe_pure_property(p,w,x,y,z)                      \
  (p)->internal_get_maybe_pure_property (ly_symbol2scm (w), x, y, z)
#define get_property_data(p,x) (p)->internal_get_property_data (ly_symbol2scm (x))
#define get_object(p,x) (p)->internal_get_object (ly_symbol2scm (x))
#define set_object(p, x, y) (p)->internal_set_object (ly_symbol2scm (x), y)
#define del_property(p,x) (p)->internal_del_property (ly_symbol2scm (x))

#ifdef DEBUG
/*
  TODO: include modification callback support here, perhaps
  through intermediate Grob::instrumented_set_property( .. __LINE__ ).
 */
#define set_property(p, x, y) (p)->instrumented_set_property (ly_symbol2scm (x), y, __FILE__, __LINE__, __FUNCTION__)
#else
#define set_property(p, x, y) (p)->internal_set_property (ly_symbol2scm (x), y)
#endif

#define LY_ASSERT_TYPE(pred, var, number)                                       \
  {                                                                     \
    if (!pred (var)) \
      {                                                                 \
        scm_wrong_type_arg_msg(mangle_cxx_identifier (__FUNCTION__).c_str(), \
                               number, var, \
                               predicate_to_typename ((void*) &pred).c_str()); \
      }                                                                 \
  }

template <class T>
T *unsmob (SCM var);

[[noreturn]] void
ly_wrong_smob_arg (bool pred (SCM), SCM var, int number, const char *fun);

// Do not call this directly.
// Use LY_ASSERT_SMOB() which supplies the function name automatically.
template <class T>
inline T *ly_assert_smob (SCM var, int number, const char *fun)
{
  T *smob = unsmob<T> (var);
  if (smob)
    return smob;

  ly_wrong_smob_arg (T::is_smob, var, number, fun);
}

// Could be just implemented using LY_ASSERT_TYPE, but this variant
// saves a slight amount of code

#define LY_ASSERT_SMOB(klass, var, number)                              \
  ly_assert_smob<klass> (var, number, __FUNCTION__)

#endif /* LILY_GUILE_MACROS_HH */
