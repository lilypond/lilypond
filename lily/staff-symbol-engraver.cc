/*
  staff-symbol-engraver.cc -- implement Staff_symbol_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "score.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "moment.hh"

/**
  Manage the staff symbol.
 */
class Staff_symbol_engraver : public Engraver { 
  Spanner *span_p_;
public:
  TRANSLATOR_DECLARATIONS(Staff_symbol_engraver);
  
protected:
  virtual ~Staff_symbol_engraver ();
  virtual void acknowledge_grob (Grob_info);
  virtual void finalize ();
  virtual void initialize ();
};


Staff_symbol_engraver::~Staff_symbol_engraver ()
{
  assert (!span_p_);
}

Staff_symbol_engraver::Staff_symbol_engraver ()
{
  span_p_ = 0;
}

void
Staff_symbol_engraver::initialize ()
{
  span_p_ = new Spanner (get_property ("StaffSymbol"));
  
  span_p_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));

  announce_grob(span_p_, SCM_EOL);
}

void
Staff_symbol_engraver::finalize ()
{
  span_p_->set_bound (RIGHT,unsmob_grob (get_property ("currentCommandColumn")));
  typeset_grob (span_p_);
  span_p_ =0;
}

void
Staff_symbol_engraver::acknowledge_grob (Grob_info s)
{
  s.grob_l_->set_grob_property ("staff-symbol", span_p_->self_scm ());

  // remove this. probly not necessary?
  s.grob_l_->add_dependency (span_p_); // UGH. UGH. UGH
}




ENTER_DESCRIPTION(Staff_symbol_engraver,
/* descr */       "create the constellation of five (default)
staff lines.",
/* creats*/       "StaffSymbol",
/* acks  */       "grob-interface",
/* reads */       "",
/* write */       "");
