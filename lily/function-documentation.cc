#include "lily-guile.hh"
#include "protected-scm.hh"
#include "string.hh"

static Protected_scm doc_hash_table ;

void ly_add_function_documentation (SCM func,
				    char const * fname,
				    char const * varlist,
				    char const * doc)
{
  if (!gh_vector_p (doc_hash_table ))
    doc_hash_table = scm_make_vector (gh_int2scm (59), SCM_EOL);

  String s = String (" - ") + "LilyPond procedure: " + fname + " " + varlist
    + "\n" + doc ;

  scm_set_procedure_property_x (func, ly_symbol2scm ("documentation"),
				scm_makfrom0str (s.to_str0 ()));
  SCM entry = gh_cons (scm_makfrom0str (varlist), scm_makfrom0str (doc));
  scm_hashq_set_x (doc_hash_table, ly_symbol2scm (fname), entry);
}


LY_DEFINE(ly_get_all_function_documentation, "ly:get-all-function-documentation",
	  0,0,0, (),
	  "Get a hash table with all lilypond Scheme extension functions.")
{
  return doc_hash_table;
}
