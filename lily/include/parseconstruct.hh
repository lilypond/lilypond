/*
  parseconstruct.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef PARSECONSTRUCT_HH
#define PARSECONSTRUCT_HH

#include "lily-proto.hh"

String * get_scriptdef(char c);
Request* get_script_req(char);
Request* get_plet_request( char c, int dur_i, int type_i ); 
Request*get_script_req(int d , Script_def*def);
Request*get_text_req(int d , Text_def*def);
Request* get_stemdir_req(int);
Request*get_grouping_req(Array<int> i_arr);
Request* get_hshift_req(int);

#endif // PARSECONSTRUCT_HH

