/*   
  function-documentation.cc -- 

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include <string.h>

#include "lily-guile.hh"
#include "protected-scm.hh"
#include "string.hh"

static Protected_scm doc_hash_table ;

void ly_add_function_documentation (SCM func,
				    char const * fname,
				    char const * varlist,
				    char const * doc)
{
  if (!strlen (doc))
    return ; 
    
  if (!is_vector (doc_hash_table ))
    doc_hash_table = scm_make_vector (scm_int2num (59), SCM_EOL);

  String s = String (" - ") + "LilyPond procedure: " + fname + " " + varlist
    + "\n" + doc ;

  scm_set_procedure_property_x (func, ly_symbol2scm ("documentation"),
				scm_makfrom0str (s.to_str0 ()));
  SCM entry = scm_cons (scm_makfrom0str (varlist), scm_makfrom0str (doc));
  scm_hashq_set_x (doc_hash_table, ly_symbol2scm (fname), entry);
}


LY_DEFINE (ly_get_all_function_documentation, "ly:get-all-function-documentation",
	  0,0,0, (),
	  "Get a hash table with all lilypond Scheme extension functions.")
{
  return doc_hash_table;
}
