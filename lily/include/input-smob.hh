/*   
  input-smob.hh -- declare input smob
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef INPUT_SMOB_HH
#define INPUT_SMOB_HH

#include "input.hh"
#include "lily-guile.hh"
#include "smobs.hh"

SCM make_input (Input spot);
Input *unsmob_input(SCM);

extern Input dummy_input_global;

#endif /* INPUT_SMOB_HH */

