/*   
  file-results.hh -- declare functions for processing one input file.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FILE_RESULTS_HH
#define FILE_RESULTS_HH

#include "lily-proto.hh"
#include "source.hh"
#include "parray.hh"
#include "scm-hash.hh"

class Input_file_results
{
public:
  Sources sources_;
  Array<String> inclusion_names_;
  Array<String> target_strings_;
  Link_array<Score> scores_;
  /* Global? prefix is bit confusing */
  Scheme_hash_table * header_;

  void do_deps ();
  void do_scores ();

  Input_file_results (String file, String init);
  ~Input_file_results ();
  
private:
  /* Make sure we never get an implicit constructor.*/
  Input_file_results ();
};

extern Input_file_results* global_input_file;

void do_one_file (String init_string, String file_string);


#endif /* FILE_RESULTS_HH */

