/*
  walkregs.cc -- implement Walker_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "clefreg.hh"
#include "localkeyreg.hh"
#include "keyreg.hh"
#include "meterreg.hh"
#include "barreg.hh"
#include "walkregs.hh"

Walker_registers::Walker_registers(Complex_walker *w)
{
    add( new Bar_register(w));
    add( new Clef_register(w));
    add( new Key_register(w));
    add( new Meter_register(w));
    add( new Local_key_register(w));
}
