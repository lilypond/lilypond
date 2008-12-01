/*
  vertical-align-engraver.cc -- implement Vertical_align_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "paper-column.hh"
#include "align-interface.hh"
#include "span-bar.hh"
#include "axis-group-interface.hh"
#include "engraver.hh"
#include "spanner.hh"
#include "pointer-group-interface.hh"
#include "grob-array.hh"

#include "translator.icc"

class Vertical_align_engraver : public Engraver
{
  Spanner *valign_;
  bool qualifies (Grob_info) const;
  SCM id_to_group_hashtab_;

public:
  TRANSLATOR_DECLARATIONS (Vertical_align_engraver);
  DECLARE_ACKNOWLEDGER (axis_group);

protected:
  virtual void derived_mark () const;
  void process_music ();
  virtual void finalize ();
  virtual void initialize ();
};

ADD_ACKNOWLEDGER (Vertical_align_engraver, axis_group);
ADD_TRANSLATOR (Vertical_align_engraver,
		/* doc */
		"Catch groups (staves, lyrics lines, etc.) and stack them"
		" vertically.",

		/* create */
		"VerticalAlignment ",

		/* read */
		"alignAboveContext "
		"alignBelowContext ",

		/* write */
		""
		);

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
      Align_interface::set_ordered (valign_);
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
    && !i.grob ()->get_parent (Y_AXIS)
    && !to_boolean (i.grob ()->get_property ("no-alignment"))
    && Axis_group_interface::has_axis (i.grob (), Y_AXIS);
}

void
Vertical_align_engraver::acknowledge_axis_group (Grob_info i)
{
  if (qualifies (i))
    {
      string id = i.context ()->id_string ();

      scm_hash_set_x (id_to_group_hashtab_, ly_string2scm (id),
		      i.grob ()->self_scm ());

      SCM before_id = i.context ()->get_property ("alignAboveContext");
      SCM after_id = i.context ()->get_property ("alignBelowContext");

      SCM before = scm_hash_ref (id_to_group_hashtab_, before_id, SCM_BOOL_F);
      SCM after = scm_hash_ref (id_to_group_hashtab_, after_id, SCM_BOOL_F);

      Grob *before_grob = unsmob_grob (before);
      Grob *after_grob = unsmob_grob (after);

      Align_interface::add_element (valign_, i.grob ());
	
      if (before_grob || after_grob)
	{
	  Grob_array *ga = unsmob_grob_array (valign_->get_object ("elements"));
	  vector<Grob*> &arr = ga->array_reference ();

	  Grob *added = arr.back ();
	  arr.pop_back ();
	  for (vsize i = 0; i < arr.size (); i++)
	    {
	      if (arr[i] == before_grob)
		{
		  arr.insert (arr.begin () + i, added);
		  break;
		}
	      else if (arr[i] == after_grob)
		{
		  arr.insert (arr.begin () + i + 1, added);
		  break;
		}
	    }
	}
    }
}
