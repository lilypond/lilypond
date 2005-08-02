/*
  ly-module.cc -- implement guile module stuff.

  source file of the GNU LilyPond music typesetter

  (c) 2002--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "lily-guile.hh"
#include "warn.hh"
#include "main.hh"
#include "string.hh"
#include "protected-scm.hh"

#define MODULE_GC_KLUDGE

#ifdef MODULE_GC_KLUDGE
Protected_scm anonymous_modules = SCM_EOL;

#endif

LY_DEFINE(ly_clear_anonymous_modules, "ly:clear-anonymous-modules",
	  0, 0, 0, (),
	  "Plug a GUILE 1.6 and 1.7 memory leak by breaking a weak reference "
	  "pointer cycle explicitly."
	  )
{
#ifdef MODULE_GC_KLUDGE
  for (SCM s = anonymous_modules;
       scm_is_pair (s);
       s = scm_cdr (s))
    {
      SCM module = scm_car (s);
      SCM closure = SCM_MODULE_EVAL_CLOSURE(module);
      SCM prop = scm_procedure_property (closure, ly_symbol2scm ("module")); 

      if (ly_c_module_p (prop))
	{
	  scm_set_procedure_property_x (closure, ly_symbol2scm ("module"),
					SCM_BOOL_F);
	}
    }

  anonymous_modules = SCM_EOL;
#endif

  return SCM_UNSPECIFIED;
}



#define FUNC_NAME __FUNCTION__

SCM
ly_make_anonymous_module (bool safe)
{
  SCM mod = SCM_EOL;
  if (!safe)
    {
      SCM maker = ly_lily_module_constant ("make-module");

      SCM scm_module = ly_lily_module_constant ("the-scm-module");
      
      mod = scm_call_0 (maker);
      scm_module_define (mod, ly_symbol2scm ("%module-public-interface"),
			 mod);
      
      ly_use_module (mod, scm_module);
      ly_use_module (mod, global_lily_module);
    }
  else
    {
      SCM proc = ly_lily_module_constant ("make-safe-lilypond-module");
      mod = scm_call_0 (proc);
    }

#ifdef MODULE_GC_KLUDGE
  anonymous_modules = scm_cons (mod, anonymous_modules);
#endif
  
  return mod;
}

SCM
ly_use_module (SCM mod, SCM used)
{
  SCM expr
    = scm_list_3 (ly_symbol2scm ("module-use!"),
		  mod,
		  scm_list_2 (ly_symbol2scm ("module-public-interface"),
			      used));

  return scm_eval (expr, global_lily_module);
}

#define FUNC_NAME __FUNCTION__

static SCM
module_define_closure_func (void *closure, SCM key, SCM val, SCM result)
{
  (void) result;
  SCM module = (SCM) closure;
  if (scm_variable_bound_p (val) == SCM_BOOL_T)
    scm_module_define (module, key, scm_variable_ref (val));
  return SCM_EOL;
}

/* Ugh signature of scm_internal_hash_fold () is inaccurate.  */
typedef SCM (*Hash_cl_func) ();

/*
  If a variable in changed in SRC, we DEST doesn't see the
  definitions.
*/
LY_DEFINE (ly_module_copy, "ly:module-copy",
	   2, 0, 0, (SCM dest, SCM src),
	   "Copy all bindings from module SRC into DEST.")
{
  SCM_VALIDATE_MODULE (1, src);
  scm_internal_hash_fold ((Hash_cl_func) & module_define_closure_func,
			  (void *) dest,
			  SCM_EOL, SCM_MODULE_OBARRAY (src));
  return SCM_UNSPECIFIED;
}

static SCM
accumulate_symbol (void *closure, SCM key, SCM val, SCM result)
{
  (void) closure;
  (void) val;
  return scm_cons (key, result);
}

SCM
ly_module_symbols (SCM mod)
{
  SCM_VALIDATE_MODULE (1, mod);

  SCM obarr = SCM_MODULE_OBARRAY (mod);
  return scm_internal_hash_fold ((Hash_cl_func) & accumulate_symbol,
				 NULL, SCM_EOL, obarr);
}

static SCM
entry_to_alist (void *closure, SCM key, SCM val, SCM result)
{
  (void) closure;
  if (scm_variable_bound_p (val) == SCM_BOOL_T)
    return scm_cons (scm_cons (key, scm_variable_ref (val)), result);
  programming_error ("unbound variable in module");
  return result;
}

LY_DEFINE (ly_module2alist, "ly:module->alist",
	   1, 0, 0, (SCM mod),
	   "Dump the contents of  module @var{mod} as an alist.")
{
  SCM_VALIDATE_MODULE (1, mod);
  SCM obarr = SCM_MODULE_OBARRAY (mod);

  return scm_internal_hash_fold ((Hash_cl_func) & entry_to_alist, NULL, SCM_EOL, obarr);
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
	   "Lookup @var{sym} in the list @var{modules}, "
	   "returning the first occurence.  "
	   "If not found, return @var{default}, or @code{#f}.")
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

void
ly_export (SCM module, SCM namelist)
{
  static SCM export_function;
  if (!export_function)
    export_function = scm_permanent_object (scm_c_lookup ("module-export!"));

  scm_call_2 (SCM_VARIABLE_REF (export_function), module, namelist);
}

void
ly_reexport_module (SCM mod)
{
  ly_export (mod, ly_module_symbols (mod));
}


#ifdef MODULE_GC_KLUDGE
static SCM
redefine_keyval (void *closure, SCM key, SCM val, SCM result)
{
  (void)closure;
  SCM new_tab = result;
  scm_hashq_set_x (new_tab, key, val);
  return new_tab;
}

/*
  UGH UGH.
  Kludge for older GUILE 1.6 versions.
 */
void
make_stand_in_procs_weak ()
{
  SCM old_tab = scm_stand_in_procs;
  SCM new_tab = scm_make_weak_key_hash_table (scm_from_int (257));
  
  new_tab = scm_internal_hash_fold ((Hash_cl_func) & redefine_keyval, NULL,
				    new_tab, old_tab);

  scm_stand_in_procs = new_tab;
}

ADD_SCM_INIT_FUNC(make_stand_in_procs_weak, make_stand_in_procs_weak);
#endif
