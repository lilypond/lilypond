/*
  custos-engraver.cc -- implement Custos_engraver

  source file of the GNU LilyPond music typesetter

  (C) 2000 Juergen Reuter <reuterj@ira.uka.de>,

  Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
*/

#include "engraver.hh"
#include "bar.hh"
#include "item.hh"
#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"
#include "musical-request.hh"

/*
  This class implements an engraver for custos symbols.
*/
class Custos_engraver : public Engraver
{
public:
  Custos_engraver();
  virtual void do_post_move_processing();
  virtual void acknowledge_element(Score_element_info);
  virtual void do_process_music ();
  virtual void process_acknowledged ();
  virtual void do_pre_move_processing ();
  virtual void do_removal_processing ();
  VIRTUAL_COPY_CONS(Translator);

private:
  Item * create_custos();
  bool custos_permitted;
  Link_array<Score_element> custos_arr_;
  Array<Pitch> pitches_;
};

Custos_engraver::Custos_engraver ()
{
  custos_permitted = false;
}


void
Custos_engraver::do_pre_move_processing()
{
  /*
    delay typeset until we're at the next moment, so we can silence custodes at the end of the piece.
   */
}

void
Custos_engraver::do_post_move_processing ()
{
  for (int i = custos_arr_.size (); i--;)
    {
      typeset_element (custos_arr_[i]);
    }
  custos_arr_.clear ();
  pitches_.clear ();

  custos_permitted = false;
}


/*
  TODO check if this works with forced bar lines?
 */
void
Custos_engraver::do_process_music ()
{
  if (gh_string_p (get_property( "whichBar")))
    custos_permitted = true;
}

void
Custos_engraver::acknowledge_element (Score_element_info info)
{
  Item *item = dynamic_cast <Item *>(info.elem_l_);
  if (item)
    {
      if (Bar::has_interface (info.elem_l_))
	custos_permitted = true;
      else if (Note_head::has_interface (info.elem_l_))
	{

	  /*
	    ideally, we'd do custos->set_parent (Y_AXIS, notehead),
	    but since the note head lives on the other system, we can't

	    So we copy the position from the note head pitch.  We
	    don't look at the staff-position, since we can't be sure
	    whether Clef_engraver already applied a vertical shift.
	  */
	  Note_req * nr = dynamic_cast<Note_req*> (info.req_l_);
	  if (nr)
	    pitches_.push (*unsmob_pitch (nr->get_mus_property ("pitch")));
	}
    }
}

void
Custos_engraver::process_acknowledged ()
{
  if (custos_permitted)
    {
      for (int i = pitches_.size (); i--;)
	{
	  Item *c = create_custos ();
	  
	  c->set_elt_property ("staff-position",
			       gh_int2scm (pitches_[i].steps ()));
	  
	}

      pitches_.clear ();
    }
}

Item* 
Custos_engraver::create_custos()
{
  SCM basicProperties = get_property ("Custos");
  Item* custos = new Item (basicProperties);
  
  announce_element (custos, 0);
  custos_arr_.push (custos);
  
  return custos;
}

void
Custos_engraver::do_removal_processing ()
{
  for (int i = custos_arr_.size (); i--;)
    {
      custos_arr_[i]->suicide ();
      typeset_element (custos_arr_[i]);
    }
  custos_arr_.clear ();
}

ADD_THIS_TRANSLATOR (Custos_engraver);

