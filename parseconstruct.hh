
#include "proto.hh"

Staff * get_new_rhythmstaff();
Voice_element * get_note_element(String,String);
Voice_element* get_rest_element(String,String);
Command *  get_bar_command(Real);
Command* get_meter_command(Real, int,int);
