/*
  parseconstruct.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef PARSECONSTRUCT_HH
#define PARSECONSTRUCT_HH

#include "proto.hh"
void set_default_duration(int *);
void get_default_duration(int *);
void set_default_octave(String);
void set_plet(int,int);
Staff * get_new_rhythmstaff();
Voice_element * get_note_element(String,int * ,int *);
Voice_element* get_rest_element(String,int *);
Staff * get_new_melodicstaff();
void add_requests( Voice_element*v, svec<Request*>&req);
Request* get_request(char);
void set_text_style(String);
Script_def* get_scriptdef(char);
Text_def*get_text(String s);
Request*get_script_req(int d , Script_def*def);
Request*get_text_req(int d , Text_def*def);


#endif // PARSECONSTRUCT_HH

