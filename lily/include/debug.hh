/*
  Argh! this sux! implementation not liblily
 */

#ifndef DEBUG_HH
#define DEBUG_HH

#include <assert.h>
#include <iostream.h>
#include "dstream.hh"
#include "real.hh"
#include "lily-proto.hh"
#include "flower-debug.hh"
#include "warn.hh"

void error_t (const String& s, Time_description const &  t_tdes);
void error_t (String const &s, const Moment &when);

// progress
extern ostream *mlog;


#endif
