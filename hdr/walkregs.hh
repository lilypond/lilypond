/*
  walkregs.hh -- declare Walker_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef WALKREGS_HH
#define WALKREGS_HH


#include "registergroup.hh"

struct Walker_registers : Register_group {
    Walker_registers(Complex_walker *w);
};

#endif // WALKREGS_HH
