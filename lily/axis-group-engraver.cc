/*   
  axis-group-engraver.cc --  implement Axis_group_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include "spanner.hh"
#include "paper-column.hh"
#include "axis-group-interface.hh"
#include "engraver.hh"
#include "engraver-group-engraver.hh"
#include "warn.hh"
#include "context.hh"

/**
   Put stuff in a Spanner with an Axis_group_interface.
   Use as last element of a context. 
 */
class Axis_group_engraver : public Engraver
{
protected:
  Spanner *staffline_;
  Link_array<Grob> elts_;
  virtual void process_music ();
  virtual void finalize ();
  virtual void acknowledge_grob (Grob_info);
  virtual void process_acknowledged_grobs ();
  virtual Spanner* get_spanner () ;
  virtual void add_element (Grob*) ;
public:  
  TRANSLATOR_DECLARATIONS (Axis_group_engraver);
};

Axis_group_engraver::Axis_group_engraver ()
{
  staffline_ = 0;
}

void
Axis_group_engraver::process_music ()
{
  if (!staffline_)
    {
      staffline_ = get_spanner ();

      Grob *  it = unsmob_grob (get_property ("currentCommandColumn"));

      staffline_->set_bound (LEFT,it);

      announce_grob (staffline_, SCM_EOL);
    }
} 

Spanner*
Axis_group_engraver::get_spanner () 
{
  return make_spanner ("VerticalAxisGroup");
}

/*
  TODO: should we junk minimumVerticalExtent/extraVerticalExtent ?  
 */

void
Axis_group_engraver::finalize ()
{
  if (!staffline_)
    return ;
  
  String type = daddy_context_->context_name ();
  SCM dims = get_property ("verticalExtent");
  
  if (is_number_pair (dims))
    staffline_->set_extent (dims, Y_AXIS);

  dims = get_property ("minimumVerticalExtent");
  if (is_number_pair (dims) )
    staffline_->set_property ("minimum-Y-extent", dims);

  dims = get_property ("extraVerticalExtent");
  if (is_number_pair (dims))
    staffline_->set_property ("extra-Y-extent", dims);

  Grob *  it = unsmob_grob (get_property ("currentCommandColumn"));

  staffline_->set_bound (RIGHT,it);

  typeset_grob (staffline_);
  staffline_ = 0;
}

void
Axis_group_engraver::acknowledge_grob (Grob_info i)
{
  elts_.push (i.grob_);
}

/*
  maybe should check if our parent is set, because we now get a
  cyclic parent relationship if we have two Axis_group_engravers in
  the context.  */
void
Axis_group_engraver::process_acknowledged_grobs ()
{
  /* UGH UGH UGH */
  for (int i=0; i < elts_.size (); i++)
    {
      Grob *par = elts_[i]->get_parent (Y_AXIS);

      if (!par || !Axis_group_interface::has_interface (par))
	{
	  if (staffline_->get_parent (Y_AXIS)
	      && staffline_->get_parent (Y_AXIS) == elts_[i])
	    {
	      String msg = _("Axis_group_engraver: vertical group already has a parent.\n"
			     "Do you have two Axis_group_engravers?\n"
			     "Killing this vertical group.");
	      staffline_->warning (msg);
	      staffline_->suicide ();
	      staffline_ = 0;
	      break ;
	    }
	  else if (elts_[i]->is_empty (Y_AXIS))
	    {
	      /*
		We have to do _something_, otherwise staff objects will
		end up with System as parent.  
	      
	      */
	      elts_[i]->set_parent (staffline_, Y_AXIS);
	    }
	  else
	    add_element (elts_[i]);
	}
    }
  elts_.clear ();
}

void
Axis_group_engraver::add_element (Grob*e)
{
  Axis_group_interface::add_element (staffline_, e);
}

/****************************************************************/

/*
  
 maybenot such a good idea after all., to put class declarations in
 .cc
 
*/

#include "hara-kiri-group-spanner.hh"
#include "rhythmic-head.hh"

class Hara_kiri_engraver : public Axis_group_engraver
{
protected:
  virtual Spanner*get_spanner ();
  virtual void acknowledge_grob (Grob_info);
  virtual void add_element (Grob *e);
public:
  TRANSLATOR_DECLARATIONS (Hara_kiri_engraver);
};

void
Hara_kiri_engraver::add_element (Grob*e)
{
  Hara_kiri_group_spanner::add_element (staffline_, e);
}


Spanner*
Hara_kiri_engraver::get_spanner () 
{
  Spanner * sp = make_spanner ("RemoveEmptyVerticalGroup");
  
  return sp;
}

void
Hara_kiri_engraver::acknowledge_grob (Grob_info i)
{
  Axis_group_engraver::acknowledge_grob (i);
  if (i.grob_->internal_has_interface (ly_symbol2scm ("rhythmic-grob-interface"))
      || i.grob_->internal_has_interface (ly_symbol2scm ("lyric-interface"))
      )
    {
      Hara_kiri_group_spanner::add_interesting_item (staffline_, i.grob_);
    }
}


Hara_kiri_engraver::Hara_kiri_engraver ()
{
}

ENTER_DESCRIPTION (Hara_kiri_engraver,
/* descr */       "Like Axis_group_engraver, but make a hara-kiri spanner, and add "
"interesting items (ie. note heads, lyric syllables and normal rests) ",
/* creats*/       "RemoveEmptyVerticalGroup",
/* accepts */     "",
/* acks  */      "grob-interface",
/* reads */       "",
/* write */       "");

ENTER_DESCRIPTION (Axis_group_engraver,
/* descr */       "Group all objects created in this context in a VerticalAxisGroup spanner.",
/* creats*/       "VerticalAxisGroup",
/* accepts */     "",
/* acks  */      "grob-interface",
/* reads */       "verticalExtent minimumVerticalExtent extraVerticalExtent",
/* write */       "");
