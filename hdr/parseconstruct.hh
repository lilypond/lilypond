/*
  parseconstruct.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef PARSECONSTRUCT_HH
#define PARSECONSTRUCT_HH

#include "proto.hh"

extern char const* defined_ch_c_l;
extern char const* req_defined_ch_c_l;
void set_default_duration(int *);
void last_duration(int n);
void set_duration_mode(String s);
void get_default_duration(int *);
void set_default_octave(String);
void set_plet(int,int);
Voice_element * get_note_element(String,int * ,int *);
Voice_element* get_rest_element(String,int *);
Voice_element* get_word_element(Text_def*, int*);
void add_requests( Voice_element*v, Array<Request*>&req);
Request* get_request(char);
void set_text_style(String);
Script_def* get_scriptdef(char);
Text_def*get_text(String s);
Request*get_script_req(int d , Script_def*def);
Request*get_text_req(int d , Text_def*def);
Request* get_stemdir_req(int);




Request*get_grouping_req(Array<int> i_arr);

#endif // PARSECONSTRUCT_HH

