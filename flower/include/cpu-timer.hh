/*
  cpu-timer.hh -- declare Cpu_timer

  source file of the Flower Library

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef CPU_TIMER_HH
#define CPU_TIMER_HH

#include <time.h>
#include "real.hh"

class Cpu_timer {
  clock_t start_clock_;
public:
  Cpu_timer ();
  void restart ();
  Real read ();
};

#endif // CPU_TIMER_HH
