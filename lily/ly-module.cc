/*
  ly-module.cc --  implement guile module stuff.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2002--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

*/

#include "string.hh"
#include "lily-guile.hh"
#include "ly-module.hh"
#include "protected-scm.hh"

#define FUNC_NAME __FUNCTION__

static int module_count;

void
ly_init_anonymous_module (void * data)
{
  (void) data;
  scm_c_use_module ("lily");
}

Protected_scm anon_modules;

SCM
ly_make_anonymous_module ()
{
  String s = "*anonymous-ly-" + to_string (module_count++) +  "*";
  SCM mod = scm_c_define_module (s.to_str0(), ly_init_anonymous_module, 0);
  anon_modules = scm_cons (mod, anon_modules);
  return mod;
}

void
ly_clear_anonymous_modules ()
{
  SCM s = anon_modules;
  anon_modules = SCM_EOL;
  
  for (; is_pair (s) ; s = ly_cdr (s))
    {
      SCM tab= scm_c_make_hash_table (2);
      /* UGH. */
      SCM_STRUCT_DATA (ly_car (s))[scm_module_index_obarray]
	= (long unsigned int) tab;
    }
}

#define FUNC_NAME __FUNCTION__

static SCM
ly_module_define (void *closure, SCM key, SCM val, SCM result)
{
  (void) result;
  SCM module = (SCM) closure;
  scm_module_define (module, key, scm_variable_ref (val));
  return SCM_EOL;
}

/* Ugh signature of scm_internal_hash_fold () is inaccurate.  */
typedef SCM (*Hash_cl_func)();

void
ly_import_module (SCM dest, SCM src)
{
  SCM_VALIDATE_MODULE (1, src);
  scm_internal_hash_fold ((Hash_cl_func) &ly_module_define, (void*) dest,
			  SCM_EOL, SCM_MODULE_OBARRAY (src));
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
  
  SCM obarr= SCM_MODULE_OBARRAY (mod);
  return scm_internal_hash_fold ((Hash_cl_func) &accumulate_symbol,
				 NULL, SCM_EOL, obarr); 
}

static SCM
entry_to_alist (void *closure, SCM key, SCM val, SCM result)
{
  (void) closure;
  return scm_cons (scm_cons (key, scm_variable_ref (val)), result);
}

SCM
ly_module_to_alist (SCM mod)
{
  SCM_VALIDATE_MODULE (1, mod);
  
  
  SCM obarr= SCM_MODULE_OBARRAY (mod);

  return scm_internal_hash_fold ((Hash_cl_func) &entry_to_alist, NULL, SCM_EOL, obarr); 
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

SCM
ly_modules_lookup (SCM modules, SCM sym)
{
  for (SCM s = ly_car (modules); SCM_MODULEP (s); s = ly_cdr (s))
    {
      SCM v = scm_sym2var (sym, scm_module_lookup_closure (s), SCM_UNDEFINED);
      if (v != SCM_UNDEFINED)
	return v;
    }
  return SCM_UNDEFINED;
}


SCM export_function;

void
ly_export (SCM module, SCM namelist)
{
  if (!export_function)
    export_function = scm_permanent_object (scm_c_lookup ("module-export!"));
  
  scm_call_2 (SCM_VARIABLE_REF (export_function), module, namelist);
}

void
ly_reexport_module (SCM mod)
{
  ly_export (mod, ly_module_symbols (mod));
}
