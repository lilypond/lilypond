/*
  chained-callback.cc -- chained callbacks.

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "lily-guile.hh"

static scm_t_bits chain_tag;

bool
is_callback_chain (SCM s)
{
  return (SCM_NIMP (s) && SCM_CELL_TYPE (s) == chain_tag);
}

SCM
callback_chain_extract_procedures (SCM chain_smob)
{
  assert (is_callback_chain (chain_smob));
  return (SCM) SCM_CELL_WORD_1(chain_smob);
}

LY_DEFINE(ly_callback_chain_p, "ly:callback-chain?",
	  1,0,0, (SCM chain),
	  "Type predicate.")
{
  return scm_from_bool (is_callback_chain (chain));
}

LY_DEFINE(ly_make_callback_chain, "ly:make-callback-chain",
	  0, 0, 1, (SCM procedures),
	  "Make a grob callback chain. @var{procedures} should be a "
	  "list of procedures taking 2 arguments.")
{
  SCM z;

  for (SCM s = procedures;
       scm_is_pair (s); s = scm_cdr (s))
    {
      SCM proc = scm_car (s);
      if (!ly_is_procedure (proc))
	{
	  scm_misc_error ("Must be a procedure: ~a",
			  "ly:make-callback-chain",
			  proc);
	}

      if (procedure_arity (proc) != 2)
	{
	  scm_misc_error ("Procedure should take 2 arguments: ~a",
			  "ly:make-callback-chain",
			  proc);
	}
    }
  
  SCM_NEWSMOB(z, chain_tag, procedures);
  return z;
}
 
int
print_callback_chain (SCM s, SCM port, scm_print_state *)
{
  scm_puts ("#<callback-chain ", port);
  scm_display (scm_cdr (s), port);
  scm_puts (" >", port);
  return 1;
}


void init_chained_callback ()
{
  chain_tag = scm_make_smob_type ("callback-chain", 0);
  scm_set_smob_mark (chain_tag, scm_markcdr);
  scm_set_smob_print (chain_tag, print_callback_chain);
};



ADD_SCM_INIT_FUNC(chain, init_chained_callback);
