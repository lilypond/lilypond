/*   
  separating-line-group-engraver.cc --  implement Separating_line_group_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separating-group-spanner.hh"
#include "separation-item.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "engraver.hh"

class Separating_line_group_engraver : public Engraver
{
protected:
  Item * break_malt_p_;
  Item * nobreak_malt_p_;
  Spanner * sep_span_p_;
  
  virtual void acknowledge_grob (Grob_info);
  virtual void initialize ();
  virtual void finalize ();
  virtual void stop_translation_timestep ();
public:
  Separating_line_group_engraver ();
  VIRTUAL_COPY_CONS (Translator);
};

Separating_line_group_engraver::Separating_line_group_engraver ()
{
  sep_span_p_ = 0;
  break_malt_p_ = 0;
  nobreak_malt_p_ =0;
}

void
Separating_line_group_engraver::initialize ()
{
  sep_span_p_ = new Spanner (get_property ("SeparatingGroupSpanner"));
  Separating_group_spanner::set_interface (sep_span_p_);
  announce_grob (sep_span_p_, 0);
  sep_span_p_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
}

void
Separating_line_group_engraver::finalize ()
{
  sep_span_p_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
  typeset_grob (sep_span_p_);
  sep_span_p_ =0;
}

void
Separating_line_group_engraver::acknowledge_grob (Grob_info i)
{
  Item * it = dynamic_cast <Item *> (i.elem_l_);
  if (it && !it->parent_l (X_AXIS))
    {
      bool ib =Item::breakable_b (it);
      Item *&p_ref_ (ib ? break_malt_p_
			      : nobreak_malt_p_);

      if (!p_ref_)
	{
	  p_ref_ = new Item
	    (get_property ("SeparationItem"));
	  
	  if (ib)
	    p_ref_->set_grob_property ("breakable", SCM_BOOL_T);
	  announce_grob (p_ref_, 0);
	}
      Separation_item::add_item (p_ref_,it);
    }
}

void
Separating_line_group_engraver::stop_translation_timestep ()
{
  if (break_malt_p_)
    {
      Separating_group_spanner::add_spacing_unit (sep_span_p_, break_malt_p_);
      
      typeset_grob (break_malt_p_);
      break_malt_p_ =0;
    }
  if (nobreak_malt_p_)
    {
      Separating_group_spanner::add_spacing_unit (sep_span_p_, nobreak_malt_p_);
      typeset_grob (nobreak_malt_p_);
      nobreak_malt_p_ =0;
    }
}



ADD_THIS_TRANSLATOR(Separating_line_group_engraver);

