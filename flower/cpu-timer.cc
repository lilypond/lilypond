/*
  cpu-timer.cc -- implement Cpu_timer

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "cpu-timer.hh"

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
  return (stop-start_clock_)/Real(CLOCKS_PER_SEC);
}
