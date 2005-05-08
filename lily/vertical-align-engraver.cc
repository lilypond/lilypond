/*
  vertical-align-engraver.cc -- implement Vertical_align_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "context.hh"
#include "paper-column.hh"
#include "align-interface.hh"
#include "span-bar.hh"
#include "axis-group-interface.hh"
#include "engraver.hh"
#include "spanner.hh"

class Vertical_align_engraver : public Engraver
{
  Spanner *valign_;
  bool qualifies (Grob_info) const;
  SCM id_to_group_hashtab_;  
  
public:
  TRANSLATOR_DECLARATIONS (Vertical_align_engraver);

protected:
  virtual void derived_mark () const;
  virtual void acknowledge_grob (Grob_info);
  virtual void process_music ();
  virtual void finalize ();
  virtual void initialize ();
};


ADD_TRANSLATOR (Vertical_align_engraver,
		"Catch groups (staffs, lyrics lines, etc.) and stack "
		"them vertically.",
		/* creats*/ "VerticalAlignment",
		/* accepts */ "",
		/* acks  */ "axis-group-interface",
		/* reads */ "",
		/* write */ "");


Vertical_align_engraver::Vertical_align_engraver ()
{
  valign_ = 0;
  id_to_group_hashtab_ = SCM_EOL;
}

void
Vertical_align_engraver::derived_mark () const
{
  scm_gc_mark (id_to_group_hashtab_); 
}

void
Vertical_align_engraver::initialize ()
{
  id_to_group_hashtab_ = scm_c_make_hash_table (11);
}


void
Vertical_align_engraver::process_music ()
{
  if (!valign_)
    {
      valign_ = make_spanner ("VerticalAlignment", SCM_EOL);
      valign_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}

void
Vertical_align_engraver::finalize ()
{
  if (valign_)
    {
      valign_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
      valign_ = 0;
    }
}

bool
Vertical_align_engraver::qualifies (Grob_info i) const
{
  int sz = i.origin_contexts ((Translator *)this).size ();

  return sz > 0 && Axis_group_interface::has_interface (i.grob ())
    && !i.grob ()->get_parent (Y_AXIS) && Axis_group_interface::has_axis (i.grob (), Y_AXIS);
}

void
Vertical_align_engraver::acknowledge_grob (Grob_info i)
{
  if (qualifies (i))
    {
      String id = i.context ()->id_string ();

      scm_hash_set_x (id_to_group_hashtab_, scm_makfrom0str (id.to_str0 ()),
		      i.grob ()->self_scm ());


      SCM before_id = i.context ()->get_property ("alignAboveContext");
      SCM after_id = i.context ()->get_property ("alignBelowContext");
      
      SCM before = scm_hash_ref (id_to_group_hashtab_,  before_id, SCM_BOOL_F);
      SCM after = scm_hash_ref (id_to_group_hashtab_,  after_id, SCM_BOOL_F);


      Align_interface::add_element (valign_, i.grob (),
				    get_property ("verticalAlignmentChildCallback"));

      if (unsmob_grob (before) || unsmob_grob (after))
	{
	  SCM elts = valign_->get_property ("elements");
	  SCM new_order = scm_cdr (elts);
	  SCM *current = &new_order;

	  for (SCM s = new_order;  scm_is_pair (s); s = scm_cdr (s))
	    {
	      if (scm_car (s) == after)
		{
		  *current = scm_cons (i.grob ()->self_scm(), s);
		  break;
		}
	      else if (scm_car (s) == before)
		{
		  scm_set_cdr_x (s, scm_cons (i.grob ()->self_scm (),
					      scm_cdr (s)));
		  break;
		}

	      current = SCM_CDRLOC (s);
	    }

	  valign_->set_property ("elements", new_order);
	}
    }
}
