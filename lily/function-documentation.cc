#include "lily-guile.hh"
#include "protected-scm.hh"

static Protected_scm doc_hash_table ;

void ly_add_function_documentation (char const * fname,
				    char const * varlist,
				    char const * doc)
{
  if (!gh_vector_p (doc_hash_table ))
    doc_hash_table = scm_make_vector (gh_int2scm (59), SCM_EOL);


  SCM entry = gh_cons (scm_makfrom0str (varlist), scm_makfrom0str (doc));
  scm_hashq_set_x (doc_hash_table, ly_symbol2scm (fname), entry);
}


LY_DEFINE(ly_get_all_function_documentation, "ly:get-all-function-documentation",
	  0,0,0, (),
	  "Get a hash table with all lilypond Scheme extension functions.")
{
  return doc_hash_table;
}
