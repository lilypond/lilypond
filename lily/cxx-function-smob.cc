/*   
  grob-callback.cc --  implement Callback smob.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "cxx-function-smob.hh"
#include "ly-smobs.icc"

static long callback_tag;

static
SCM mark_smob (SCM)
{
  return SCM_EOL;
}

static int
print_smob (SCM, SCM port, scm_print_state *)
{
  scm_puts ("#<encapsulated C++ function>", port);
  return 1;
}

static
scm_sizet free_smob (SCM)
{
  return 0;
}


SCM
cxx_function_type_p (SCM x)
{
  return (SCM_CELL_TYPE(x) == callback_tag) ? SCM_BOOL_T : SCM_BOOL_F; 
}

void init_cxx_function_smobs()
{
  callback_tag = scm_make_smob_type_mfpe ("callback", 0,
					  mark_smob, free_smob,
					  print_smob, 0);

  scm_make_gsubr ("c++-function?", 1, 0, 0, (Scheme_function_unknown) cxx_function_type_p);
}

SCM
smobify_cxx_function (Cxx_function cb)
{
  SCM z;
  
  SCM_NEWCELL(z);
  SCM_SETCDR (z, (SCM)cb);
  SCM_SETCAR (z, (SCM)callback_tag);

  return z;
}


Cxx_function
unsmob_cxx_function (SCM x)
{
  
  if (SCM_NIMP (x) && SCM_CELL_TYPE(x) == callback_tag)
    return (Cxx_function) SCM_CELL_WORD_1(x);
  else
    return 0;
}

