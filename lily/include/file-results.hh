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

class Input_file_settings
{
public:
  Sources sources_;
  Array<String> inclusion_names_;
  Array<String> target_strings_;
  Link_array<Score> scores_;
  Scheme_hash_table * global_header_;

  void do_deps( );
  void do_scores();

  Input_file_settings (String file,String init);
  ~Input_file_settings();
};

extern Input_file_settings* global_input_file;

void do_one_file (String init_string, String file_string);


#endif /* FILE_RESULTS_HH */

