/*
  cpu-timer.cc -- implement Cpu_timer

  source file of the Flower Library

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "cpu-timer.hh"

#include <unistd.h>
// nextstep
#ifndef CLOCKS_PER_SEC
#ifdef CLK_TCK
#define CLOCKS_PER_SEC CLK_TCK
#elif defined _SC_CLK_TCK
#define CLOCKS_PER_SEC sysconf (_SC_CLK_TCK)
#else
#error cannot determine CLOCKS_PER_SEC
#endif
#endif

Cpu_timer::Cpu_timer ()
{
  restart ();
}
void
Cpu_timer::restart ()
{
  start_clock_ = clock ();
}

Real
Cpu_timer::read ()
{
  clock_t stop = clock ();
  return (stop - start_clock_) / Real (CLOCKS_PER_SEC);
}
