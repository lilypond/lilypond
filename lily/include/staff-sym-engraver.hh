/*
  staff-sym-engraver.hh -- declare Staff_symbol_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef STAFF_SYM_GRAV_HH
#define STAFF_SYM_GRAV_HH
#include "engraver.hh"
#include "moment.hh"

/**
  Manage the staff symbol.
 */
class Staff_symbol_engraver : public Engraver { 
    Staff_symbol *span_p_;
public:
  VIRTUAL_COPY_CONS(Translator);
  Staff_symbol_engraver();
  
protected:
  virtual ~Staff_symbol_engraver();
  virtual void fill_staff_info (Staff_info&);
  virtual void do_removal_processing();
  virtual void do_creation_processing();
	
};
#endif // STAFF_SYM_GRAV_HH
