/*   
  grob-callback.cc --  implement Callback smob.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "cxx-function-smob.hh"
#include "ly-smobs.icc"

static scm_t_bits callback_tag;

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

static size_t
free_smob (SCM)
{
  return 0;
}

LY_DEFINE(cxx_function_type_p, "c++-function?", 1, 0, 0, (SCM x),
	  "Is this an encapsulated C++ function ?")
{
  return (SCM_CELL_TYPE (x)) == callback_tag ? SCM_BOOL_T : SCM_BOOL_F; 
}

void init_cxx_function_smobs ()
{
  callback_tag = scm_make_smob_type ("callback", 0);
  scm_set_smob_mark (callback_tag, mark_smob);
  scm_set_smob_free (callback_tag, free_smob);
  scm_set_smob_print (callback_tag, print_smob);
  scm_set_smob_equalp (callback_tag, 0);
}

SCM
smobify_cxx_function (Cxx_function cb)
{
  SCM z;
  SCM_NEWSMOB(z,callback_tag, cb) ;
  return z;
}


Cxx_function
unsmob_cxx_function (SCM x)
{
  
  if (SCM_NIMP (x) && SCM_CELL_TYPE (x) == callback_tag)
    return (Cxx_function) SCM_CELL_WORD_1 (x);
  else
    return 0;
}

