#if 0
/*   
  grob-callback.cc --  implement Callback smob.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "grob-callback.hh"

static SCM callback_tag;

static
SCM mark_smob (SCM)
{
  return SCM_EOL;
}

static int
print_smob (SCM, SCM port, scm_print_state *)
{
  scm_puts ("#<Callback>", port);
  return 1;
}

static
scm_sizet free_smob (SCM)
{
  return 0;
}

static
void start_callback_smobs()
{
  callback_tag = scm_make_smob_type_mfpe ("callback", 0,
					  mark_smob, free_smob,
					  print_smob, 0);
}


SCM
smobify_callback (Grob_callback cb )
{
  SCM z;
  
  SCM_NEWCELL(z);
  SCM_SETCDR (z, (SCM)cb);
  SCM_SETCAR (z, (SCM)callback_tag);

  return z;
}
  
ADD_SCM_INIT_FUNC(callback, start_callback_smobs);
#endif
