#include "proto.hh"

void set_default_duration(String);
void set_default_pitch(String);
Staff * get_new_rhythmstaff();
Voice_element * get_note_element(String,String);
Voice_element* get_rest_element(String,String);
Staff * get_new_melodicstaff();
void add_requests( Voice_element*v, svec<Request*>&req);
Request* get_request(char);

