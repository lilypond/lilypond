/*
  lily-guile-macros.hh -- declare

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#if (__GNUC__ > 2)
/* Unreliable with gcc-2.x
   FIXME: should add check for x86 as well?  */
#define CACHE_SYMBOLS
#endif

#ifdef CACHE_SYMBOLS

/* Using this trick we cache the value of scm_str2symbol ("fooo") where
   "fooo" is a constant string. This is done at the cost of one static
   variable per ly_symbol2scm() use, and one boolean evaluation for
   every call.

   The overall speedup of lily is about 5% on a run of wtk1-fugue2.  */
#define ly_symbol2scm(x)						\
  ({									\
    static SCM cached;							\
    /* We store this one locally, since G++ -O2 fucks up else */	\
    SCM value = cached;							\
    if (__builtin_constant_p ((x)))					\
      {									\
	if (!cached)							\
	  value = cached = scm_gc_protect_object (scm_str2symbol ((x))); \
      }									\
    else								\
      value = scm_str2symbol ((char *) (x));				\
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
 
/*
  Make TYPE::FUNC available as a Scheme function.
*/
std::string mangle_cxx_identifier (std::string);
#define MAKE_SCHEME_CALLBACK(TYPE, FUNC, ARGCOUNT)			\
  SCM TYPE ::FUNC ## _proc;						\
  void									\
  TYPE ## _ ## FUNC ## _init_functions ()				\
  {									\
    std::string id = mangle_cxx_identifier (std::string (#TYPE) + "::" + std::string (#FUNC)); \
    TYPE ::FUNC ## _proc = scm_c_define_gsubr (id.c_str(),			\
					       (ARGCOUNT), 0, 0,	\
					       (Scheme_function_unknown) TYPE::FUNC); \
    scm_c_export (id.c_str (), NULL);					\
  }									\
									\
  ADD_SCM_INIT_FUNC (TYPE ## _ ## FUNC ## _callback,			\
		     TYPE ## _ ## FUNC ## _init_functions);

void
ly_add_function_documentation (SCM proc, char const *fname,
			       char const *varlist,
			       char const *doc);

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
#define get_object(x) internal_get_object (ly_symbol2scm (x))
#define set_property(x, y) internal_set_property (ly_symbol2scm (x), y)
#define set_object(x, y) internal_set_object (ly_symbol2scm (x), y)

#endif /* LILY_GUILE_MACROS_HH */
