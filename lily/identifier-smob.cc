/*   
identifier-smob.cc -- implement glue to pass Scheme expressions off as
identifiers.

source file of the GNU LilyPond music typesetter

(c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

*/
#include "identifier-smob.hh"
/*
  C&P from example/box.c
 */

scm_t_bits package_tag;

/* Print a textual represenation of the smob to a given port.  */
static int
print_box (SCM b, SCM port, scm_print_state *pstate)
{
  SCM value = SCM_CELL_OBJECT_1 (b);

  scm_puts ("#<packaged object ", port);
  scm_write (value, port);
  scm_puts (">", port);

  /* Non-zero means success.  */
  return 1;
}


/* This defines the primitve `make-box', which returns a new smob of
   type `box', initialized to `#f'.  */
LY_DEFINE(package_identifier, "ly-id", 1,0,0, (SCM arg),
	  "Package a Scheme object, so it is treated as an identifier.")
{
  /* This macro creates the new objects, stores the value `#f' into it
     and returns it to the caller.  */
  SCM_RETURN_NEWSMOB (package_tag, arg);
}


/* This is the primitive `box-ref' which returns the object stored in
   the box.  */
SCM
unpack_identifier(SCM box)
{
  if (SCM_IMP(box) || SCM_CELL_TYPE(box) != package_tag)
    return SCM_UNDEFINED;
  
  return SCM_CELL_OBJECT_1 (box);
}

static void
init_box_type (void)
{
  package_tag = scm_make_smob_type ("box", 0);
  scm_set_smob_mark (package_tag, scm_markcdr);
  scm_set_smob_print (package_tag, print_box);
}
ADD_SCM_INIT_FUNC(package, init_box_type); 
