//
// midi-global.hh -- declare global (sic) stuff for mi2mu
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MIDI_GLOBAL_HH
#define MIDI_GLOBAL_HH

#include "string.hh"

#define monitor_p_g &cout
enum Verbose { QUIET_ver, BRIEF_ver, NORMAL_ver, VERBOSE_ver, DEBUG_ver };
extern Verbose level_ver;
#ifdef NPRINT
#define dtor if ( 0 ) *monitor_p_g
#define mtor if ( 0 ) *monitor_p_g
#define vtor if ( level_ver >= VERBOSE_ver ) *monitor_p_g
#define btor if ( level_ver >= BRIEF_ver ) *monitor_p_g
#define qtor if ( level_ver >= QUIET_ver ) *monitor_p_g
#else
#define dtor if ( level_ver >= DEBUG_ver ) *monitor_p_g
#define vtor if ( level_ver >= VERBOSE_ver ) *monitor_p_g
#define mtor if ( level_ver >= NORMAL_ver ) *monitor_p_g
#define btor if ( level_ver >= BRIEF_ver ) *monitor_p_g
#define qtor if ( level_ver >= QUIET_ver ) *monitor_p_g
#endif

extern Sources* source_l_g;
void message( String message_str, char const* context_ch_C );
void warning( String message_str, char const* context_ch_C );
void error( String message_str, char const* context_ch_C );

String mi2mu_version_str();

#endif // MIDI_GLOBAL_HH

