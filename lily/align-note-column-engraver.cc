/*   
  align-note-column-engraver.cc --  implement Align_note_column_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "grace-align-item.hh"
#include "note-column.hh"

/**
   catch notes, and put them in a row.
 */
class Align_note_column_engraver: public Engraver
{
  Axis_align_item * align_item_p_;
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
}

void
Align_note_column_engraver::do_creation_processing ()
{
  align_item_p_ = new Grace_align_item;
  // needed  for setting font size.
  announce_element (Score_element_info (align_item_p_, 0));
}

void
Align_note_column_engraver::do_removal_processing ()
{
  typeset_element (align_item_p_);
  align_item_p_ =0;
}

void
Align_note_column_engraver::acknowledge_element (Score_element_info inf)
{
  if (Note_column * n = dynamic_cast<Note_column*> (inf.elem_l_))
    {
      align_item_p_->add_element (n);
    }
}

ADD_THIS_TRANSLATOR(Align_note_column_engraver);
