/*
  staff-symbol-engraver.cc -- implement Staff_symbol_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  VIRTUAL_COPY_CONS(Translator);
  Staff_symbol_engraver();
  
protected:
  virtual ~Staff_symbol_engraver();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_removal_processing();
  virtual void do_creation_processing();
};


Staff_symbol_engraver::~Staff_symbol_engraver()
{
  assert (!span_p_);
}

Staff_symbol_engraver::Staff_symbol_engraver()
{
  span_p_ = 0;
}

void
Staff_symbol_engraver::do_creation_processing()
{
  span_p_ = new Spanner (get_property ("StaffSymbol"));
  
  span_p_->set_bound(LEFT, unsmob_element (get_property ("currentCommandColumn")));

  announce_element (span_p_, 0);
}

void
Staff_symbol_engraver::do_removal_processing()
{
  span_p_->set_bound(RIGHT,unsmob_element (get_property ("currentCommandColumn")));
  typeset_element (span_p_);
  span_p_ =0;
}

void
Staff_symbol_engraver::acknowledge_element (Score_element_info s)
{
  s.elem_l_->set_elt_property ("staff-symbol", span_p_->self_scm ());
  s.elem_l_->add_dependency (span_p_); // UGH. UGH. UGH 
}


ADD_THIS_TRANSLATOR(Staff_symbol_engraver);

