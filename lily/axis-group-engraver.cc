/*   
  axis-group-engraver.cc --  implement Axis_group_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include "spanner.hh"
#include "paper-column.hh"
#include "axis-group-interface.hh"
#include "engraver.hh"
#include "engraver-group-engraver.hh"

/**
   Put stuff in a Spanner with an Axis_group_interface.
   Use as last element of a context. 
 */
class Axis_group_engraver : public Engraver
{
protected:
  Spanner *staffline_p_;
  Link_array<Score_element> elts_;
  virtual void do_creation_processing();
  virtual void do_removal_processing();
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual Spanner* get_spanner_p () const;
  virtual void add_element (Score_element*) ;
public:  
  VIRTUAL_COPY_CONS(Translator);
  Axis_group_engraver ();
};

ADD_THIS_TRANSLATOR(Axis_group_engraver);

Axis_group_engraver::Axis_group_engraver ()
{
  staffline_p_ = 0;
}

void
Axis_group_engraver::do_creation_processing ()
{
  staffline_p_ = get_spanner_p ();
  Axis_group_interface::set_interface (staffline_p_);
  Axis_group_interface::set_axes (staffline_p_, Y_AXIS, Y_AXIS);

  Score_element *  it = unsmob_element (get_property ("currentCommandColumn"));

  staffline_p_->set_bound(LEFT,it);

  announce_element (staffline_p_, 0);
}

Spanner*
Axis_group_engraver::get_spanner_p () const
{
  return new Spanner (get_property ("basicVerticalAxisGroupProperties"));
}

void
Axis_group_engraver::do_removal_processing ()
{
  String type = daddy_grav_l ()->type_str_ ;
  SCM dims = get_property ((type  + "VerticalExtent").ch_C());
  
  if (gh_pair_p (dims) && gh_number_p (gh_car (dims))
      && gh_number_p (gh_cdr (dims)))
    {
      staffline_p_->set_extent_callback (&Score_element::preset_extent, Y_AXIS);
      staffline_p_->set_elt_property ("extent-Y", dims);
    }

  dims = get_property ((type + "MinimumVerticalExtent").ch_C());
  if (gh_pair_p (dims) && gh_number_p (gh_car (dims))
      && gh_number_p (gh_cdr (dims)))
    staffline_p_->set_elt_property ("minimum-extent-Y", dims);

  dims = get_property ((type + "ExtraVerticalExtent").ch_C());
  if (gh_pair_p (dims) && gh_number_p (gh_car (dims))
      && gh_number_p (gh_cdr (dims)))
    staffline_p_->set_elt_property ("extra-extent-Y", dims);

  Score_element *  it = unsmob_element (get_property ("currentCommandColumn"));


  staffline_p_->set_bound(RIGHT,it);

  typeset_element (staffline_p_);
  staffline_p_ = 0;
}

void
Axis_group_engraver::acknowledge_element (Score_element_info i)
{
  elts_.push (i.elem_l_);
}

/*
  maybe should check if our parent_l is set, because we now get a
  cyclic parent relationship if we have two Axis_group_engravers in
  the context.  */
void
Axis_group_engraver::process_acknowledged ()
{
  /* UGH UGH UGH */
  for (int i=0; i < elts_.size (); i++)
    {
      Score_element *par = elts_[i]->parent_l (Y_AXIS);

      if ((!par || !Axis_group_interface::has_interface (par))
	  && ! elts_[i]->empty_b (Y_AXIS))
	add_element (elts_[i]);
    }
  elts_.clear ();
}

void
Axis_group_engraver::add_element (Score_element*e)
{
  Axis_group_interface::add_element (staffline_p_, e);
}

////////////////////////////////////////////////////////
// maybenot such a good idea after all., to put classes in .cc

#include "hara-kiri-group-spanner.hh"
#include "rhythmic-head.hh"

class Hara_kiri_engraver : public Axis_group_engraver
{
protected:
  virtual Spanner*get_spanner_p ()const;
  virtual void acknowledge_element (Score_element_info);
  virtual void add_element (Score_element *e);
public:
  VIRTUAL_COPY_CONS(Translator);
};

void
Hara_kiri_engraver::add_element (Score_element*e)
{
  Hara_kiri_group_spanner::add_element (staffline_p_, e);
}


Spanner*
Hara_kiri_engraver::get_spanner_p () const
{
  Spanner * sp = new Spanner (get_property ("basicHaraKiriVerticalGroupspannerProperties"));
  Hara_kiri_group_spanner::set_interface (sp);
  return sp;
}

void
Hara_kiri_engraver::acknowledge_element (Score_element_info i)
{
  Axis_group_engraver::acknowledge_element (i);
  if (Rhythmic_head::has_interface (i.elem_l_)
      || i.elem_l_->has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    {
      Hara_kiri_group_spanner::add_interesting_item (staffline_p_, i.elem_l_);
    }
}
ADD_THIS_TRANSLATOR(Hara_kiri_engraver);
