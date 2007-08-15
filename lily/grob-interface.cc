/*
  grob-interface.cc -- implement graphic objects interface

  source file of the GNU LilyPond music typesetter

  (c) 2002--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "grob-interface.hh"

#include "grob.hh"
#include "international.hh"
#include "protected-scm.hh"
#include "std-string.hh"
#include "warn.hh"

void add_interface (char const *symbol,
		    char const *descr,
		    char const *vars)
{
  SCM s = ly_symbol2scm (symbol);
  SCM d = scm_makfrom0str (descr);
  SCM l = parse_symbol_list (vars);

  ly_add_interface (s, d, l);
}

void
check_interfaces_for_property (Grob const *me, SCM sym)
{
  if (sym == ly_symbol2scm ("meta"))
    {
      /*
	otherwise we get in a nasty recursion loop.
      */
      return;
    }

  SCM ifs = me->interfaces ();

  SCM all_ifaces = ly_all_grob_interfaces ();
  bool found = false;
  for (; !found && scm_is_pair (ifs); ifs = scm_cdr (ifs))
    {
      SCM iface = scm_hashq_ref (all_ifaces, scm_car (ifs), SCM_BOOL_F);
      if (iface == SCM_BOOL_F)
	{
	  string msg = to_string (_f ("Unknown interface `%s'",
				      ly_symbol2string (scm_car (ifs)).c_str ()));
	  programming_error (msg);
	  continue;
	}

      found = found || (scm_c_memq (sym, scm_caddr (iface)) != SCM_BOOL_F);
    }

  if (!found)
    {
      string str = to_string (_f ("Grob `%s' has no interface for property `%s'",
				  me->name ().c_str (),
				  ly_symbol2string (sym).c_str ()));
      programming_error (str);
    }
}
