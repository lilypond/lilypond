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
  Custos_engraver ();
  virtual void start_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();
  virtual void finalize ();
  VIRTUAL_COPY_CONS (Translator);

private:
  Item * create_custos ();
  bool custos_permitted;
  Link_array<Grob> custos_arr_;
  Array<Pitch> pitches_;
};

Custos_engraver::Custos_engraver ()
{
  custos_permitted = false;
}


void
Custos_engraver::stop_translation_timestep ()
{
  /*
    delay typeset until we're at the next moment, so we can silence custodes at the end of the piece.
   */
}

void
Custos_engraver::start_translation_timestep ()
{
  for (int i = custos_arr_.size (); i--;)
    {
      typeset_grob (custos_arr_[i]);
    }
  custos_arr_.clear ();
  pitches_.clear ();

  custos_permitted = false;
}


void
Custos_engraver::acknowledge_grob (Grob_info info)
{
  Item *item = dynamic_cast <Item *> (info.elem_l_);
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
Custos_engraver::create_grobs ()
{
  if (gh_string_p (get_property ("whichBar")))
    custos_permitted = true;
  
  if (custos_permitted)
    {
      for (int i = pitches_.size (); i--;)
	{
	  Item *c = create_custos ();

	  int p = pitches_[i].steps ();
	  SCM c0 = get_property ("centralCPosition");
	  if (gh_number_p (c0))
	    p += gh_scm2int (c0);

	  
	  c->set_grob_property ("staff-position",
				gh_int2scm (p));
	  
	}

      pitches_.clear ();
    }
}

Item* 
Custos_engraver::create_custos ()
{
  SCM basicProperties = get_property ("Custos");
  Item* custos = new Item (basicProperties);
  
  announce_grob (custos, 0);
  custos_arr_.push (custos);
  
  return custos;
}

void
Custos_engraver::finalize ()
{
  for (int i = custos_arr_.size (); i--;)
    {
      custos_arr_[i]->suicide ();
      typeset_grob (custos_arr_[i]);
    }
  custos_arr_.clear ();
}

ADD_THIS_TRANSLATOR (Custos_engraver);

