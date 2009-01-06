/*
  cpu-timer.hh -- declare Cpu_timer

  source file of the Flower Library

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef CPU_TIMER_HH
#define CPU_TIMER_HH

#include <ctime>
using namespace std;

#include "real.hh"

class Cpu_timer
{
  clock_t start_clock_;
public:
  Cpu_timer ();
  void restart ();
  Real read ();
};

#endif // CPU_TIMER_HH
