//
// midi-main.hh -- global (sic) m2m stuff 
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>
#include "string.hh"

#define monitor_p_g &cout
enum Verbose { QUIET_ver, BRIEF_ver, NORMAL_ver, VERBOSE_ver, DEBUG_ver };
extern Verbose level_ver;
#ifdef NPRINT
#define dtor if ( 0 ) *monitor_p_g
#define mtor if ( 0 ) *monitor_p_g
#else
#define dtor if ( level_ver >= DEBUG_ver ) *monitor_p_g
#define vtor if ( level_ver >= VERBOSE_ver ) *monitor_p_g
#define mtor if ( level_ver >= NORMAL_ver ) *monitor_p_g
#define btor if ( level_ver >= BRIEF_ver ) *monitor_p_g
#define qtor if ( level_ver >= QUIET_ver ) *monitor_p_g
#endif

extern Source* source_l_g;
extern bool no_triplets_bo_g;
void message( String message_str, char const* context_ch_c_l );
void warning( String message_str, char const* context_ch_c_l );
void error( String message_str, char const* context_ch_c_l );

String version_str();
