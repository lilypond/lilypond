/*
  repeat-engraver.cc -- implement Repeat_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "repeat-engraver.hh"
#include "bar.hh"
#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "command-request.hh"
#include "time-description.hh"
#include "engraver-group.hh"
#include "repeated-music.hh"
#include "time-description.hh"
#include "volta-spanner.hh"
#include "note-column.hh"

ADD_THIS_TRANSLATOR (Repeat_engraver);

Repeat_engraver::Repeat_engraver ()
{
}

bool
Repeat_engraver::do_try_music (Music* m)
{
  if (Repeated_music* r = dynamic_cast<Repeated_music *> (m))
    {
      r->unfold_b_ = get_property ("unfoldRepeats", 0).to_bool ();
      if (r->unfold_b_)
        return true;
 
      Moment stop_mom = now_moment () + r->repeat_p_->duration () 
        + r->alternative_p_->music_p_list_p_->top ()->duration ();
      Moment alt_mom = now_moment () + r->repeat_p_->duration ();
      /*
        TODO: 
	  figure out what we don't want.

	  we don't want to print more than one set of
	  |: :| and volta brackets on one staff.

	  counting nested repeats, it seems safest to forbid
	  two pieces of alternative music to start at the same time.
       */
      for (int i = 0; i < alternative_start_mom_arr_.size (); i++)
        if (alternative_start_mom_arr_[i] == alt_mom)
	  return false;
      repeated_music_arr_.push (r);
      stop_mom_arr_.push (stop_mom);
      for (PCursor<Music*> i (r->alternative_p_->music_p_list_p_->top ()); i.ok (); i++)
        {
	  alternative_music_arr_.push (i.ptr ());
	  alternative_start_mom_arr_.push (alt_mom);
	  alt_mom += i->duration ();
	  alternative_stop_mom_arr_.push (alt_mom);
	}
      return true;
    }
  return false;
}

void
Repeat_engraver::acknowledge_element (Score_element_info i)
{
  Moment now = now_moment ();
  if (Note_column *c = dynamic_cast<Note_column *> (i.elem_l_))
    {
      for (int i = 0; i < volta_p_arr_.size (); i++)
        if ((now >= alternative_start_mom_arr_[i]) && volta_p_arr_[i])
	  volta_p_arr_[i]->add_column (c);
    }
  if (Bar *c = dynamic_cast<Bar*> (i.elem_l_))
    {
      for (int i = 0; i < volta_p_arr_.size (); i++)
        if ((now >= alternative_start_mom_arr_[i]) && volta_p_arr_[i])
	  volta_p_arr_[i]->add_column (c);
    }
}

void
Repeat_engraver::do_removal_processing ()
{
  for (int i = 0; i < bar_p_arr_.size (); i++)
    if (bar_p_arr_[i])
      typeset_element (bar_p_arr_[i]);
  for (int i = 0; i < volta_p_arr_.size (); i++)
    if (volta_p_arr_[i])
      typeset_element (volta_p_arr_[i]);
}

void
Repeat_engraver::do_process_requests ()
{  
  Moment now = now_moment ();
  for (int i = bar_p_arr_.size (); i < repeated_music_arr_.size (); i++)
    {
      Bar* bar_p = new Bar;
      bar_p-> type_str_ = "|:";
      bar_p_arr_.push (bar_p);
      if (now > Moment (0))
	announce_element (Score_element_info (bar_p, repeated_music_arr_[i])); 
    }
  for (int i = 0; i < bar_p_arr_.size (); i++)
    {
      if (!bar_p_arr_[i] && (now >= stop_mom_arr_[i]))
        {
	  Bar* bar_p = new Bar;
	  bar_p-> type_str_ = ":|";
	  bar_p_arr_[i] = bar_p;
	  announce_element (Score_element_info (bar_p, repeated_music_arr_[i]));
	}
    }
  int bees = volta_p_arr_.size ();
  for (int i = volta_p_arr_.size (); i < alternative_music_arr_.size (); i++)
    {
      Volta_spanner* v = new Volta_spanner;
      if (i == alternative_music_arr_.size () - 1)
        v->last_b_ = true;
      Text_def* t = new Text_def;
      t->text_str_ = to_str (i - bees + 1);
      v->number_p_.set_p (t);
      volta_p_arr_.push (v);
      announce_element (Score_element_info (v, alternative_music_arr_[i]));
    }
}

void 
Repeat_engraver::do_pre_move_processing ()
{
  Moment now = now_moment ();
  for (int i = bar_p_arr_.size (); i--; )
    {
      if (bar_p_arr_[i])
        {
	  if (now > Moment (0))
	    typeset_element (bar_p_arr_[i]);
	  else
	    delete bar_p_arr_[i];
	  bar_p_arr_[i] = 0;
	}
      if (now >= stop_mom_arr_[i])
	{
	  bar_p_arr_.del (i);
	  stop_mom_arr_.del (i);
	  repeated_music_arr_.del (i);
	}
    }
  for (int i = volta_p_arr_.size (); i--; )
    {
      if (now >= alternative_stop_mom_arr_[i])
        {
	  if (volta_p_arr_[i])
	    {
	      typeset_element (volta_p_arr_[i]);
	      volta_p_arr_[i] = 0;
	    }
	  volta_p_arr_.del (i);
	  alternative_music_arr_[i] = 0;
	  alternative_music_arr_.del (i);
	  alternative_start_mom_arr_.del (i);
	  alternative_stop_mom_arr_.del (i);
	 }
    }
}

void 
Repeat_engraver::do_post_move_processing ()
{
}

