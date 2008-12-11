/*
  ly-module.cc -- implement guile module stuff.

  source file of the GNU LilyPond music typesetter

  (c) 2002--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "lily-guile.hh"
#include "warn.hh"
#include "main.hh"
#include "std-string.hh"
#include "protected-scm.hh"

#ifdef MODULE_GC_KLUDGE
Protected_scm anonymous_modules = SCM_EOL;
bool perform_gc_kludge;
#endif

void
clear_anonymous_modules ()
{
#ifdef MODULE_GC_KLUDGE
  for (SCM s = anonymous_modules;
       scm_is_pair (s);
       s = scm_cdr (s))
    {
      SCM module = scm_car (s);
      SCM closure = SCM_MODULE_EVAL_CLOSURE (module);
      SCM prop = scm_procedure_property (closure, ly_symbol2scm ("module"));

      if (ly_is_module (prop))
	{
	  scm_set_procedure_property_x (closure, ly_symbol2scm ("module"),
					SCM_BOOL_F);
	}
    }

  anonymous_modules = SCM_EOL;
#endif
}

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
  if (perform_gc_kludge)
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



SCM
ly_module_symbols (SCM mod)
{
  SCM_VALIDATE_MODULE (1, mod);

  SCM obarr = SCM_MODULE_OBARRAY (mod);
  return ly_hash_table_keys (obarr);
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

LY_DEFINE (ly_module_2_alist, "ly:module->alist",
	   1, 0, 0, (SCM mod),
	   "Dump the contents of module @var{mod} as an alist.")
{
  SCM_VALIDATE_MODULE (1, mod);
  SCM obarr = SCM_MODULE_OBARRAY (mod);

  return scm_internal_hash_fold ((Hash_closure_function) & entry_to_alist, NULL, SCM_EOL, obarr);
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
  /*
    Ugh, ABI breakage for 1.6.5: scm_stand_in_procs is a hashtab from
    1.6.5 on.
   */
  if (scm_is_pair (scm_stand_in_procs))
    {
      return; 
    }
      
  if (scm_weak_key_hash_table_p (scm_stand_in_procs) == SCM_BOOL_T)
    {
#if (SCM_MINOR_VERSION == 7) 
      perform_gc_kludge = false;
#endif
      return; 
    }

  
  perform_gc_kludge = true;
  
  
  SCM old_tab = scm_stand_in_procs;
  SCM new_tab = scm_make_weak_key_hash_table (scm_from_int (257));

  new_tab = scm_internal_hash_fold ((Hash_closure_function) & redefine_keyval,
				    NULL,
				    new_tab,
				    old_tab);

  scm_stand_in_procs = new_tab;
}

ADD_SCM_INIT_FUNC (make_stand_in_procs_weak, make_stand_in_procs_weak);
#endif
