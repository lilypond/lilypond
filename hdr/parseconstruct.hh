#include "proto.hh"

template<class T>
struct svec;

void set_default_duration(String);
void set_default_pitch(String);
Staff * get_new_rhythmstaff();
Voice_element * get_note_element(String,String);
Voice_element* get_rest_element(String,String);
Command *  get_bar_command(Real);
Staff * get_new_melodicstaff();
void add_requests( Voice_element*v, svec<Request*>&req);
Command* get_meterchange_command( int,int);
Command* get_meter_command( Real,int,int);
Command* get_skip_command( int,Real);
Command* get_key_interpret_command(svec<String>);
Request* get_request(char);
Command*get_clef_interpret_command(String w);
