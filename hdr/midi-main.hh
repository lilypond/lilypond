//
// midi-main.hh -- global (sic) m2m stuff 
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "string.hh"
extern Source* source_l_g;
void message( String message_str, char const* context_ch_c_l );
void warning( String message_str, char const* context_ch_c_l );
void error( String message_str, char const* context_ch_c_l );
