#include  "protected-scm.hh"
#include "grob-interface.hh"
#include "lily-guile.hh"
#include "grob.hh"
#include "warn.hh"

Protected_scm all_ifaces;

void add_interface (const char * symbol,
		    const char * descr,
		    const char * vars)
{
  SCM s = ly_symbol2scm (symbol);
  SCM d = gh_str02scm (descr);
  SCM l = parse_symbol_list (vars);


  ly_add_interface(s,d,l);
}

SCM
ly_add_interface (SCM a, SCM b, SCM c)
{
  SCM_ASSERT_TYPE (gh_symbol_p(a), a, SCM_ARG1, __FUNCTION__, "symbol");
  SCM_ASSERT_TYPE (gh_string_p(b), b, SCM_ARG2, __FUNCTION__, "string");  
  SCM_ASSERT_TYPE (gh_list_p(c), c,  SCM_ARG3, __FUNCTION__, "list of syms");    
  if (!gh_vector_p (all_ifaces))
    {
      all_ifaces = scm_make_vector (gh_int2scm (40), SCM_EOL);
    }

  SCM entry = scm_list_n (a, b, c, SCM_UNDEFINED);

  scm_hashq_set_x (all_ifaces, a, entry);


  return SCM_UNSPECIFIED;
}


SCM
ly_all_grob_interfaces ()
{
  return all_ifaces;
}

void
init_iface_funcs ()
{
  scm_c_define_gsubr ("ly-add-interface", 3, 0, 0,
		      (Scheme_function_unknown)ly_add_interface);
  scm_c_define_gsubr ("ly-all-grob-interfaces", 0, 0, 0,
		      (Scheme_function_unknown)ly_all_grob_interfaces);
}

ADD_SCM_INIT_FUNC(giface, init_iface_funcs);

void
check_interfaces_for_property (Grob const *me, SCM sym)
{
  if (sym == ly_symbol2scm ("meta"))
    {
      /*
	otherwise we get in a nasty recursion loop.
       */
      return ;

    }
  SCM ifs =  me->get_grob_property ("interfaces");


  bool found = false;
  for (; !found && gh_pair_p (ifs); ifs =gh_cdr (ifs))
    {
      SCM iface = scm_hashq_ref (all_ifaces , gh_car (ifs), SCM_BOOL_F);
      if (iface == SCM_BOOL_F)
	{
	  String msg = to_str ("Unknown interface `%s'",
			       ly_scm2string (scm_symbol_to_string (gh_car(ifs))).ch_C());
	  programming_error (msg);
	  continue;
	}

      found= found || (scm_memq (sym, gh_caddr (iface)) != SCM_BOOL_F);
    }

  if (!found)
    {
     String str = to_str("Grob %s has no interface for property %s",
			 me->name ().ch_C(),
			 ly_symbol2string(sym).ch_C());
     programming_error (str);
    }
}
