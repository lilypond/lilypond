/*
  lily/module-scheme.cc -- implement module bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "ly-module.hh"

#include "warn.hh"
#include "main.hh"
#include "std-string.hh"


/*
  If a variable is changed in SRC, then DEST doesn't see the
  definitions.
*/

static SCM
module_define_closure_func (void *closure, SCM key, SCM val, SCM result)
{
  (void) result;
  SCM module = (SCM) closure;
  if (scm_variable_bound_p (val) == SCM_BOOL_T)
    scm_module_define (module, key, scm_variable_ref (val));
  return SCM_EOL;
}

LY_DEFINE (ly_module_copy, "ly:module-copy",
	   2, 0, 0, (SCM dest, SCM src),
	   "Copy all bindings from module @var{src} into @var{dest}.")
{
#define FUNC_NAME __FUNCTION__
  SCM_VALIDATE_MODULE (1, src);
  scm_internal_hash_fold ((Hash_closure_function) & module_define_closure_func,
			  (void *) dest,
			  SCM_EOL, SCM_MODULE_OBARRAY (src));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_clear_anonymous_modules, "ly:clear-anonymous-modules",
	   0, 0, 0, (),
	   "Plug a GUILE 1.6 and 1.7 memory leak by breaking a weak"
	   " reference pointer cycle explicitly.")
{
#ifdef MODULE_GC_KLUDGE
  clear_anonymous_modules ();
#endif

  return SCM_UNSPECIFIED;
}

/* Lookup SYM, but don't give error when it is not defined.  */
SCM
ly_module_lookup (SCM module, SCM sym)
{
#define FUNC_NAME __FUNCTION__
  SCM_VALIDATE_MODULE (1, module);

  return scm_sym2var (sym, scm_module_lookup_closure (module), SCM_BOOL_F);
#undef FUNC_NAME
}

/* Lookup SYM in a list of modules, which do not have to be related.
   Return the first instance. */
LY_DEFINE (ly_modules_lookup, "ly:modules-lookup",
	   2, 1, 0,
	   (SCM modules, SCM sym, SCM def),
	   "Look up @var{sym} in the list @var{modules},"
	   " returning the first occurence.  If not found, return"
	   " @var{def} or @code{#f} if @var{def} isn't specified.")
{
  for (SCM s = modules; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM mod = scm_car (s);
      SCM v = ly_module_lookup (mod, sym);
      if (SCM_VARIABLEP (v) && SCM_VARIABLE_REF (v) != SCM_UNDEFINED)
	return scm_variable_ref (v);
    }

  if (def != SCM_UNDEFINED)
    return def;
  return SCM_BOOL_F;
}
