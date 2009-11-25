/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SMOB_FREE_RETURN_VAL
#define SMOB_FREE_RETURN_VAL(CL) 0
#endif

#ifndef SCM_PACK
#define SCM_PACK(x) ((SCM) x)
#endif

#ifndef SCM_UNPACK
#define SCM_UNPACK(x) (x)
#endif

/* Unreliable with gcc-2.x
   FIXME: should add check for x86 as well?  */
#define CACHE_SYMBOLS

#ifdef CACHE_SYMBOLS

/* this lets us "overload" macros such as get_property to take
   symbols as well as strings */
inline SCM
scm_or_str2symbol (char const *c) { return scm_str2symbol (c); }

inline SCM
scm_or_str2symbol (SCM s) {
  assert (scm_is_symbol (s));
  return s;
}

/* Using this trick we cache the value of scm_str2symbol ("fooo") where
   "fooo" is a constant string. This is done at the cost of one static
   variable per ly_symbol2scm() use, and one boolean evaluation for
   every call.
 */
#define ly_symbol2scm(x)						\
  ({									\
    static SCM cached;							\
    /* We store this one locally, since G++ -O2 fucks up else */	\
    SCM value = cached;							\
    if (__builtin_constant_p ((x)))					\
      {									\
	if (!cached)							\
	  value = cached = scm_gc_protect_object (scm_or_str2symbol (x)); \
      }									\
    else								\
      value = scm_or_str2symbol (x);					\
    value;								\
  })
#else
inline SCM ly_symbol2scm (char const *x) { return scm_str2symbol ((x)); }
#endif

/*
  TODO: rename me to ly_c_lily_module_eval

  we don't have to protect the result; it's already part of the
  exports list of the module.
*/

#define ly_lily_module_constant(x)					\
  ({									\
    static SCM cached;							\
    /* We store this one locally, since G++ -O2 fucks up else */	\
    SCM value = cached;							\
    if (__builtin_constant_p ((x)))					\
      {									\
	if (!cached)							\
	  value = cached = scm_eval (scm_str2symbol (x),		\
				     global_lily_module);		\
      }									\
    else								\
      value = scm_eval (scm_str2symbol (x), global_lily_module);	\
    value;								\
  })

/*
  Adds the NAME as a Scheme function, and a variable to store the SCM
  version of the function in the static variable NAME_proc
*/
#define DECLARE_SCHEME_CALLBACK(NAME, ARGS)	\
  static SCM NAME ARGS;				\
  static SCM NAME ## _proc
#define ADD_TYPE_PREDICATE(func, type_name) \
  void \
  func ## _type_adder ()			\
  {\
    ly_add_type_predicate ((Type_predicate_ptr)func, type_name);	\
  }\
  ADD_SCM_INIT_FUNC(func ## _type_adder_ctor, \
		    func ## _type_adder);
#define ADD_TYPE_PREDICATE(func, type_name) \
  void \
  func ## _type_adder ()			\
  {\
    ly_add_type_predicate ((Type_predicate_ptr)func, type_name);	\
  }\
  ADD_SCM_INIT_FUNC(func ## _type_adder_ctor, \
		    func ## _type_adder);

string mangle_cxx_identifier (string);

void ly_add_type_predicate (void *ptr, string name);
string predicate_to_typename (void *ptr);

/*
  Make TYPE::FUNC available as a Scheme function.
*/
#define MAKE_SCHEME_CALLBACK_WITH_OPTARGS(TYPE, FUNC, ARGCOUNT, OPTIONAL_COUNT, DOC) \
  SCM TYPE ::FUNC ## _proc;						\
  void									\
  TYPE ## _ ## FUNC ## _init_functions ()				\
  {									\
    string cxx = string (#TYPE) + "::" + string (#FUNC); \
    string id = mangle_cxx_identifier (cxx); \
    TYPE ::FUNC ## _proc = scm_c_define_gsubr (id.c_str(),			\
					       (ARGCOUNT-OPTIONAL_COUNT), OPTIONAL_COUNT, 0,	\
					       (Scheme_function_unknown) TYPE::FUNC); \
    ly_add_function_documentation (TYPE :: FUNC ## _proc, id.c_str(), "", \
				   DOC);				\
    scm_c_export (id.c_str (), NULL);					\
  }									\
									\
  ADD_SCM_INIT_FUNC (TYPE ## _ ## FUNC ## _callback,			\
		     TYPE ## _ ## FUNC ## _init_functions);

#define MAKE_DOCUMENTED_SCHEME_CALLBACK(TYPE, FUNC, ARGCOUNT, DOC)		\
  MAKE_SCHEME_CALLBACK_WITH_OPTARGS(TYPE, FUNC, ARGCOUNT, 0, DOC);

#define MAKE_SCHEME_CALLBACK(TYPE, FUNC, ARGCOUNT)			\
  MAKE_SCHEME_CALLBACK_WITH_OPTARGS(TYPE,FUNC,ARGCOUNT, 0, "");

void ly_add_function_documentation (SCM proc, string fname, string varlist, string doc);
void ly_check_name (string cxx, string fname);

#define ADD_SCM_INIT_FUNC(name, func)		\
  class name ## _scm_initter			\
  {						\
  public:					\
    name ## _scm_initter ()			\
    {						\
      add_scm_init_func (func);			\
    }						\
  }						\
    _ ## name ## _scm_initter;

/* end define */

#define LY_DEFINE_WITHOUT_DECL(INITPREFIX, FNAME, PRIMNAME, REQ, OPT, VAR, \
			       ARGLIST, DOCSTRING)			\
  SCM FNAME ## _proc;							\
  void									\
  INITPREFIX ## init ()							\
  {									\
    FNAME ## _proc = scm_c_define_gsubr (PRIMNAME, REQ, OPT, VAR,	\
					 (Scheme_function_unknown) FNAME); \
    ly_check_name (#FNAME, PRIMNAME);\
    ly_add_function_documentation (FNAME ## _proc, PRIMNAME, #ARGLIST,	\
				   DOCSTRING);				\
    scm_c_export (PRIMNAME, NULL);					\
  }									\
  ADD_SCM_INIT_FUNC (INITPREFIX ## init_unique_prefix, INITPREFIX ## init); \
  SCM									\
  FNAME ARGLIST

#define LY_DEFINE(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING)	\
  SCM FNAME ARGLIST;							\
  LY_DEFINE_WITHOUT_DECL (FNAME, FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, \
			  DOCSTRING)

#define LY_DEFINE_MEMBER_FUNCTION(CLASS, FNAME, PRIMNAME, REQ, OPT, VAR, \
				  ARGLIST, DOCSTRING)			\
  SCM FNAME ARGLIST;							\
  LY_DEFINE_WITHOUT_DECL (CLASS ## FNAME, CLASS::FNAME, PRIMNAME, REQ, OPT, \
			  VAR, ARGLIST, DOCSTRING)

#define get_property(x) internal_get_property (ly_symbol2scm (x))
#define get_property_data(x) internal_get_property_data (ly_symbol2scm (x))
#define get_object(x) internal_get_object (ly_symbol2scm (x))
#define set_object(x, y) internal_set_object (ly_symbol2scm (x), y)
#define del_property(x) internal_del_property (ly_symbol2scm (x))

#ifndef NDEBUG
/*
  TODO: include modification callback support here, perhaps
  through intermediate Grob::instrumented_set_property( .. __LINE__ ).
 */
#define set_property(x, y) instrumented_set_property (ly_symbol2scm (x), y, __FILE__, __LINE__, __FUNCTION__)
#else
#define set_property(x, y) internal_set_property (ly_symbol2scm (x), y)
#endif



#define LY_ASSERT_TYPE(pred, var, number)					\
  {									\
    if (!pred (var)) \
      {									\
	scm_wrong_type_arg_msg(mangle_cxx_identifier (__FUNCTION__).c_str(), \
			       number, var, \
			       predicate_to_typename ((void*) &pred).c_str()); \
      }									\
  }

#define LY_ASSERT_SMOB(klass, var, number) LY_ASSERT_TYPE(klass::unsmob, var, number)



#endif /* LILY_GUILE_MACROS_HH */
