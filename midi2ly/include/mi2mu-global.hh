//
// mi2mu-global.hh -- declare global (sic) stuff for mi2mu
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef MI2MU_GLOBAL_HH
#define MI2MU_GLOBAL_HH

#include "string.hh"
#include "proto.hh"

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

String mi2mu_version_str();
extern bool no_timestamps_b_g;;

#endif // MI2MU_GLOBAL_HH

