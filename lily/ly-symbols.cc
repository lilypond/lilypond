/*   
  ly-symbols.cc --  implement scheme symbols
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "lily-guile.hh"

/*
  storage
 */
#undef LY_SYMBOLS_HH
#define extern
#include "ly-symbols.hh"

#undef DECLARE_LY_SYMBOL
#undef LY_SYMBOLS_HH
#define DECLARE_LY_SYMBOL(a) a ## _scm_sym = ly_symbol (#a); scm_permanent_object(a ## _scm_sym)

/*
  initialisations.
 */
void
init_symbols ()
{
#include "ly-symbols.hh"
}


