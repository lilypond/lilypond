/*   
ly-module.cc --  implement guile module stuff.

source file of the GNU LilyPond music typesetter

(c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "string.hh"
#include "lily-guile.hh"
#include "ly-modules.hh"
#include "protected-scm.hh"

#define FUNC_NAME __FUNCTION__

static int module_count;

void
ly_init_anonymous_module (void * data)
{
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

  for (; gh_pair_p (s) ; s = gh_cdr (s))
    {
      scm_vector_fill_x (SCM_MODULE_OBARRAY(gh_car(s)), SCM_EOL);
    }
}

#define FUNC_NAME __FUNCTION__

void
ly_copy_module_variables (SCM dest, SCM src)
{
  SCM_VALIDATE_MODULE (1, src);

  SCM obarr= SCM_MODULE_OBARRAY(src);
  SCM syms = SCM_EOL;

  for (int i = 0;  i < SCM_VECTOR_LENGTH (obarr); i++)
    {
      for( SCM s = scm_vector_ref(obarr, SCM_MAKINUM (i));
	   gh_pair_p (s); s = gh_cdr (s))
	{
	  scm_module_define (dest, gh_caar (s), scm_variable_ref (gh_cdar(s)));
	}
    }
}

SCM
ly_module_symbols (SCM mod)
{
  SCM_VALIDATE_MODULE (1, mod);
  
  SCM obarr= SCM_MODULE_OBARRAY(mod);
  SCM syms = SCM_EOL;

  for (int i = 0;  i < SCM_VECTOR_LENGTH (obarr); i++)
    {
      for( SCM s = scm_vector_ref(obarr, SCM_MAKINUM (i));
	   gh_pair_p (s); s = gh_cdr (s))
	{
	  syms = scm_cons (gh_caar (s), syms);
	}
    }
  return syms;
}



SCM
ly_module_to_alist (SCM mod)
{
  SCM_VALIDATE_MODULE (1, mod);
  
  
  SCM obarr= SCM_MODULE_OBARRAY(mod);
  SCM alist = SCM_EOL;

  for (int i = 0;  i < SCM_VECTOR_LENGTH (obarr); i++)
    {
      for( SCM s = scm_vector_ref(obarr, SCM_MAKINUM (i));
	   gh_pair_p (s); s = gh_cdr (s))
	{
	  alist = scm_acons (gh_caar (s), scm_variable_ref (gh_cdar (s)),
							    alist);
	}
    }
  return alist;
}

/*
  Lookup SYM, but don't give error when it is not defined.
 */
SCM
ly_module_lookup (SCM module, SCM sym)
{
#define FUNC_NAME __FUNCTION__
  SCM_VALIDATE_MODULE (1, module);

  return scm_sym2var (sym, scm_module_lookup_closure (module), SCM_BOOL_F);
}

SCM export_function ;

void
ly_export (SCM module, SCM namelist)
{
  if (!export_function)
    {
      export_function = scm_permanent_object (scm_c_lookup ("module-export!"));
    }
  
  scm_call_2 (SCM_VARIABLE_REF (export_function),
	      module, namelist);
}

void
ly_reexport_module (SCM mod)
{
  ly_export (mod, ly_module_symbols (mod));
}
