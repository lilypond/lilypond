/*   
  file-results.hh -- declare functions for processing one input file.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FILE_RESULTS_HH
#define FILE_RESULTS_HH
#include "flower-proto.hh"

void do_one_file (String init_str, String file_str);
extern Scheme_hash_table *global_header_p;
extern Array<String> target_str_global_array;
extern Array<String> inclusion_global_array;
extern Link_array<Score> score_global_array;
void do_scores ();
void clear_scores ();


#endif /* FILE_RESULTS_HH */

