/*   
  file-results.hh -- declare functions for processing one input file.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FILE_RESULTS_HH
#define FILE_RESULTS_HH
#include "flower-proto.hh"

void do_one_file (String init_string, String file_string);
extern Scheme_hash_table *global_header;
extern Array<String> target_string_globals;
extern Array<String> inclusion_globals;
extern Link_array<Score> score_globals;
void do_scores ();
void clear_scores ();


#endif /* FILE_RESULTS_HH */

