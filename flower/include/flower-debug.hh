/*
  flower-debug.hh -- declare global constants relating to debug dumps

  source file of the Flower Library

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef FLOWER_DEBUG_HH
#define FLOWER_DEBUG_HH

#include "dstream.hh"

extern Dstream *flower_dstream;

#ifdef NPRINT
#define	DEBUG_OUT if (0) *flower_dstream
#else
#define DEBUG_OUT if (flower_dstream) \
	flower_dstream->identify_as (__PRETTY_FUNCTION__)
#endif

void set_debug (Dstream *ds);

#endif // FLOWER_DEBUG_HH
