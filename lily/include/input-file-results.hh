/*   
  file-results.hh -- declare functions for processing one input file.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FILE_RESULTS_HH
#define FILE_RESULTS_HH

#include "lily-proto.hh"
#include "source.hh"
#include "parray.hh"
#include "protected-scm.hh"

class Input_file_results
{
public:
  Sources sources_;
  Array<String> inclusion_names_;
  Array<String> target_strings_;
  Protected_scm header_;

  int book_count_;
  int score_count_;
  
  void do_deps (String);

  Input_file_results (String init, String in, String out);
  ~Input_file_results ();
  
private:
  /* Make sure we never get an implicit constructor.*/
  Input_file_results ();
};

extern Input_file_results* global_input_file;

void do_one_file (char const* file);

#endif /* FILE_RESULTS_HH */

