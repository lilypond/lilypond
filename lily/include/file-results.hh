/*   
  file-results.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#ifndef FILE_RESULTS_HH
#define FILE_RESULTS_HH
#include "fproto.hh"

void do_one_file (String init_str, String file_str);

extern Array<String> target_str_global_array;
extern Array<String> inclusion_global_array;
extern Link_array<Score> score_global_array;
void do_scores ();
void clear_scores ();


#endif /* FILE_RESULTS_HH */

