//
// midi2ly-global.hh -- declare global stuff for midi2ly
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef MIDI2LY_GLOBAL_HH
#define MIDI2LY_GLOBAL_HH

#include <iostream.h>

#include "string.hh"
#include "flower-proto.hh"

#define monitor_p_g &cout
enum Verbose { QUIET_ver, BRIEF_ver, NORMAL_ver, VERBOSE_ver, DEBUG_ver };
extern Verbose level_ver;
#if 0 // NPRINT
    // not what i want, all output goes through tors.
    // set verbosity level.
    #define LOGOUT(threshold) if  (0) *monitor_p_g
#else
    #define LOGOUT(threshold) if  (level_ver >= threshold) *monitor_p_g
#endif

extern Sources* source_l_g;
// huh?
void message (String message_str); //, char const* context_ch_C);
void warning (String message_str); //, char const* context_ch_C);
void error (String message_str); //, char const* context_ch_C);

String midi2ly_version_str();
extern bool no_timestamps_b_g;;

#endif // MIDI2LY_GLOBAL_HH

