/*   
  axis-group-engraver.cc --  implement Axis_group_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  Link_array<Grob> elts_;
  virtual void initialize ();
  virtual void finalize ();
  virtual void acknowledge_grob (Grob_info);
  virtual void process_acknowledged_grobs ();
  virtual Spanner* get_spanner_p () const;
  virtual void add_element (Grob*) ;
public:  
TRANSLATOR_DECLARATIONS(Axis_group_engraver );
};



Axis_group_engraver::Axis_group_engraver ()
{
  staffline_p_ = 0;
}

void
Axis_group_engraver::initialize ()
{
  staffline_p_ = get_spanner_p ();

  Grob *  it = unsmob_grob (get_property ("currentCommandColumn"));

  staffline_p_->set_bound (LEFT,it);

  announce_grob(staffline_p_, SCM_EOL);
}

Spanner*
Axis_group_engraver::get_spanner_p () const
{
  return new Spanner (get_property ("VerticalAxisGroup"));
}

void
Axis_group_engraver::finalize ()
{
  String type = daddy_grav_l ()->type_str_ ;
  SCM dims = get_property ("verticalExtent");
  
  if (gh_pair_p (dims) && gh_number_p (ly_car (dims))
      && gh_number_p (ly_cdr (dims)))
    {
      staffline_p_->set_extent (Grob::preset_extent_proc, Y_AXIS);
      staffline_p_->set_grob_property ("extent-Y", dims);
    }

  dims = get_property ("minimumVerticalExtent");
  if (gh_pair_p (dims) && gh_number_p (ly_car (dims))
      && gh_number_p (ly_cdr (dims)))
    staffline_p_->set_grob_property ("minimum-extent-Y", dims);

  dims = get_property ("extraVerticalExtent");
  if (gh_pair_p (dims) && gh_number_p (ly_car (dims))
      && gh_number_p (ly_cdr (dims)))
    staffline_p_->set_grob_property ("extra-extent-Y", dims);

  Grob *  it = unsmob_grob (get_property ("currentCommandColumn"));


  staffline_p_->set_bound (RIGHT,it);

  typeset_grob (staffline_p_);
  staffline_p_ = 0;
}

void
Axis_group_engraver::acknowledge_grob (Grob_info i)
{
  elts_.push (i.grob_l_);
}

/*
  maybe should check if our parent_l is set, because we now get a
  cyclic parent relationship if we have two Axis_group_engravers in
  the context.  */
void
Axis_group_engraver::process_acknowledged_grobs ()
{
  /* UGH UGH UGH */
  for (int i=0; i < elts_.size (); i++)
    {
      Grob *par = elts_[i]->get_parent (Y_AXIS);

      if ((!par || !Axis_group_interface::has_interface (par))
	  && ! elts_[i]->empty_b (Y_AXIS))
	add_element (elts_[i]);
    }
  elts_.clear ();
}

void
Axis_group_engraver::add_element (Grob*e)
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
  virtual void acknowledge_grob (Grob_info);
  virtual void add_element (Grob *e);
public:
  TRANSLATOR_DECLARATIONS(Hara_kiri_engraver);
};

void
Hara_kiri_engraver::add_element (Grob*e)
{
  Hara_kiri_group_spanner::add_element (staffline_p_, e);
}


Spanner*
Hara_kiri_engraver::get_spanner_p () const
{
  Spanner * sp = new Spanner (get_property ("HaraKiriVerticalGroup"));

  return sp;
}

void
Hara_kiri_engraver::acknowledge_grob (Grob_info i)
{
  Axis_group_engraver::acknowledge_grob (i);
  if (Rhythmic_head::has_interface (i.grob_l_)
      || i.grob_l_->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    {
      Hara_kiri_group_spanner::add_interesting_item (staffline_p_, i.grob_l_);
    }
}

Hara_kiri_engraver::Hara_kiri_engraver(){}

ENTER_DESCRIPTION(Hara_kiri_engraver,
/* descr */       "Like Axis_group_engraver, but make a hara kiri spanner, and add
interesting items (ie. note heads, lyric syllables and normal rests)
",
/* creats*/       "HaraKiriVerticalGroup",
/* acks  */       "grob-interface",
/* reads */       "",
/* write */       "");

ENTER_DESCRIPTION(Axis_group_engraver,
/* descr */       "Group all objects created in this context in a VerticalAxisGroup spanner.",
/* creats*/       "VerticalAxisGroup",
/* acks  */       "grob-interface",
/* reads */       "verticalExtent minimumVerticalExtent extraVerticalExtent",
/* write */       "");
