//
// mi2mu.hh -- generic mi2mu include file
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef M2M_HH
#define M2M_HH

// yes, i know this hurts the dependency calc, however, 
// having includes in headers sucks, and long trial and error
// include lists also suck.
// if you want less dependecies, break lib/exe down in smaller
// modules.

#include <assert.h>
#include <iostream.h>
#include <fstream.h>
#include <limits.h>
#include <ctype.h>
#include <time.h>

#include "proto.hh"
#include "plist.hh"
#include "debug.hh"
#ifdef mtor
#undef mtor
#endif

#include "string.hh"
#include "string-convert.hh"

#include "lgetopt.hh"

#include "moment.hh"
#include "duration-convert.hh"
#include "duration.hh"
#include "source-file.hh"
#include "source.hh"

// mustn-t do, these get touched!
// #include "fversion.hh"
// #include "version.hh"

#include "midi-global.hh"

#include "lily-stream.hh"
#include "midi-event.hh"
#include "midi-score.hh"
#include "midi-track.hh"
#include "midi-voice.hh"
#include "my-midi-lexer.hh"
#include "my-midi-parser.hh"
#include "track-column.hh"

#endif // M2M_HH

