/*
  bar-number-grav.cc -- implement Bar_number_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "lily-guile.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "side-position-interface.hh"
#include "staff-symbol.hh"
#include "item.hh"
#include "moment.hh"
#include "engraver.hh"
#include "protected-scm.hh"

class Bar_number_engraver : public Engraver
{
protected:
  Item* text_p_;

  Protected_scm staffs_;

protected:
  virtual void do_creation_processing ();
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info);
  void create_items();
  void do_process_music ();
public:
  VIRTUAL_COPY_CONS(Translator);
  Bar_number_engraver();
};

void
Bar_number_engraver::do_process_music ()
{
  // todo include (&&!time->cadenza_b_ )
  SCM bn = get_property("currentBarNumber");
  SCM smp = get_property ("measurePosition");
  Moment mp =  (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
  
  if (gh_number_p (bn) &&
      !mp && now_mom () > Moment (0))
    {
      create_items ();

      // guh.
      text_p_->set_elt_property ("text",
				 ly_str02scm (to_str (gh_scm2int (bn)).ch_C()));
    }
}

ADD_THIS_TRANSLATOR(Bar_number_engraver);

Bar_number_engraver::Bar_number_engraver ()
{
  text_p_ =0;
  staffs_ = SCM_EOL;
}

void
Bar_number_engraver::do_creation_processing ()
{
}


					       
void
Bar_number_engraver::acknowledge_element (Score_element_info inf)
{
  Score_element * s = inf.elem_l_;
  if (Staff_symbol::has_interface (s))
    {
      staffs_ = gh_cons (inf.elem_l_->self_scm_, staffs_);
    }
  else if (text_p_
	   && dynamic_cast<Item*> (s)
	   && s->get_elt_property ("break-align-symbol") == ly_symbol2scm ("Left_edge_item"))
    {
      /*
	By default this would land on the Paper_column -- so why
	doesn't it work when you leave this out?  */
      text_p_->set_parent (s, X_AXIS);
    }
}

void 
Bar_number_engraver::do_pre_move_processing ()
{
  if (text_p_)
    {
      text_p_->set_elt_property ("side-support-elements", staffs_);
      typeset_element (text_p_);
      text_p_ =0;
    }
}


void
Bar_number_engraver::create_items ()
{
  if (text_p_)
    return;

  SCM b = get_property ("basicBarNumberProperties");
  text_p_ = new Item (b);
  Side_position::set_axis(text_p_,Y_AXIS);

  SCM prop = get_property ("barNumberDirection");
  if (!isdir_b (prop))
    {
      prop = gh_int2scm (UP);
    }
  text_p_->set_elt_property ("direction", prop);

  SCM padding = get_property ("barNumberScriptPadding");
  if (gh_number_p(padding))
    {
      text_p_->set_elt_property ("padding", padding);
    }
  else
    {
      text_p_
	->set_elt_property ("padding",
			    gh_double2scm(paper_l ()->get_var ("interline")));
    }


  announce_element (Score_element_info (text_p_, 0));
}

