/*   
  align-note-column-engraver.cc --  implement Align_note_column_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "grace-align-item.hh"
#include "align-interface.hh"
#include "note-column.hh"
#include "warn.hh"
#include "directional-element-interface.hh"
#include "side-position-interface.hh"
#include "local-key-item.hh"
#include "paper-def.hh"

/**
   Catch notes, and put them in a row. Used for aligning grace notes.
 */
class Align_note_column_engraver: public Engraver
{
  Item * align_item_p_;
  Score_element * now_column_l_;
  Score_element * accidental_l_;

  virtual void process_acknowledged ();
  virtual void do_post_move_processing ();
  virtual void do_creation_processing ();
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
public:
  VIRTUAL_COPY_CONS(Translator);
  Align_note_column_engraver ();
};

Align_note_column_engraver::Align_note_column_engraver()
{
  align_item_p_ =0;
  now_column_l_ =0;
  accidental_l_ =0;
}

void
Align_note_column_engraver::do_creation_processing ()
{
  align_item_p_ = new Item (get_property ("GraceAlignment"));
  Grace_align_item::set_interface (align_item_p_);
  Side_position::set_axis (align_item_p_, X_AXIS);
  Side_position::set_direction (align_item_p_, LEFT);
  
  // needed  for setting font size.
  announce_element (align_item_p_, 0);
}

void
Align_note_column_engraver::do_removal_processing ()
{
  SCM al = get_property ("graceAlignPosition");
  if (isdir_b (al))
    {
      Direction d = to_dir (al);
      Directional_element_interface::set (align_item_p_,d);
    }
  
  typeset_element (align_item_p_);
  align_item_p_ =0;
}

void
Align_note_column_engraver::acknowledge_element (Score_element_info inf)
{
  if (Note_column::has_interface(inf.elem_l_))
    {
      now_column_l_ =inf.elem_l_;
    }
  else if (Local_key_item::has_interface (inf.elem_l_))
    {
      accidental_l_ = inf.elem_l_;
    }
}
void
Align_note_column_engraver::process_acknowledged ()
{
  if (now_column_l_ && accidental_l_)
    {
      
      /* Can't inspect  width of Local_key_item, since

	 A. it may not be fully built

	 B. it has no pscore_l_ field.


	 UGH UGH: separate note-spacing into  separate class,  and
	 use that to space grace notes.	 
      */
      SCM grsp = get_property ("graceAccidentalSpace") ;
      if (gh_number_p(grsp))
	{
	  /*
	    ugh.
	  */
	  Real extra_space = gh_scm2double(grsp);
	  SCM e = gh_cons (gh_double2scm (-extra_space),
			   gh_double2scm (0.0));
	  now_column_l_->set_elt_property ("extra-space", e);
	}
    }

  if (now_column_l_)
    {
      Align_interface::add_element (align_item_p_,now_column_l_);
      now_column_l_ =0;
    }
}

void
Align_note_column_engraver::do_post_move_processing ()
{
  now_column_l_ =0;
  accidental_l_ =0;
}

ADD_THIS_TRANSLATOR(Align_note_column_engraver);

