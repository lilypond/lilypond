/*
  repeat-engraver.cc -- implement Repeat_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "repeat-engraver.hh"
#include "bar.hh"
#include "bar-engraver.hh"
#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "command-request.hh"
#include "time-description.hh"
#include "engraver-group.hh"
#include "repeated-music.hh"
#include "time-description.hh"
#include "volta-spanner.hh"
#include "note-column.hh"
#include "paper-def.hh"

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
 
      Music_sequence* alt = r->alternative_p_;
      Moment stop_mom = now_moment () + r->repeat_p_->duration ();
      for (PCursor<Music*> i (alt->music_p_list_p_->top ()); i.ok () && (i != alt->music_p_list_p_->bottom ()); i++)
	{
	  stop_mom += i->duration ();
	  if (dynamic_cast<Simultaneous_music *> (alt))
	    break;
	}
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
      for (PCursor<Music*> i (alt->music_p_list_p_->top ()); i.ok (); i++)
        {
	  alternative_music_arr_.push (i.ptr ());
	  alternative_start_mom_arr_.push (alt_mom);
	  alternative_stop_mom_arr_.push (alt_mom + i->duration ());
	  if (!dynamic_cast<Simultaneous_music *> (alt))
	    alt_mom += i->duration ();
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
        if (volta_p_arr_[i] && (now >= alternative_start_mom_arr_[i]))
	  volta_p_arr_[i]->add_column (c);
    }
  if (Bar *c = dynamic_cast<Bar*> (i.elem_l_))
    {
      for (int i = 0; i < volta_p_arr_.size (); i++)
        if (volta_p_arr_[i] && (now >= alternative_start_mom_arr_[i]))
	  volta_p_arr_[i]->add_column (c);
    }
}

void
Repeat_engraver::do_removal_processing ()
{
  for (int i = 0; i < volta_p_arr_.size (); i++)
    if (volta_p_arr_[i])
      typeset_element (volta_p_arr_[i]);
}

void
Repeat_engraver::do_process_requests ()
{  
  Moment now = now_moment ();
  Bar_engraver* bar_engraver_l = dynamic_cast <Bar_engraver*>
    (daddy_grav_l ()->get_simple_translator ("Bar_engraver"));
  for (int i = bar_b_arr_.size (); i < repeated_music_arr_.size (); i++)
    {
      if (bar_engraver_l && (now > Moment (0)))
	bar_engraver_l->request_bar ("|:");
      bar_b_arr_.push (true);
    }
  for (int i = 0; i < bar_b_arr_.size (); i++)
    {
      if (!bar_b_arr_[i] && (now >= stop_mom_arr_[i]))
        {
	  if (bar_engraver_l)
	    bar_engraver_l->request_bar (":|");
	}
    }
  int bees = volta_p_arr_.size ();
  for (int i = volta_p_arr_.size (); i < alternative_music_arr_.size (); i++)
    {
      Volta_spanner* v = new Volta_spanner;
      Scalar prop = get_property ("voltaVisibility", 0);
      v->visible_b_ = prop.to_bool ();
      if (i == alternative_music_arr_.size () - 1)
        v->last_b_ = true;
      Text_def* t = new Text_def;
      t->text_str_ = to_str (i - bees + 1) + ".";
      v->number_p_.set_p (t);
      volta_p_arr_.push (v);
      announce_element (Score_element_info (v, alternative_music_arr_[i]));
    }
}

void 
Repeat_engraver::do_pre_move_processing ()
{
  Moment now = now_moment ();
  for (int i = bar_b_arr_.size (); i--; )
    {
      if (bar_b_arr_[i])
	bar_b_arr_[i] = false;
      if (now >= stop_mom_arr_[i])
	{
	  bar_b_arr_.del (i);
	  stop_mom_arr_.del (i);
	  repeated_music_arr_.del (i);
	}
    }
  for (int i = volta_p_arr_.size (); i--; )
    {
      if (volta_p_arr_[i] && (now >= alternative_stop_mom_arr_[i])
	  && (volta_p_arr_[i]->column_arr_.size () >= 1))
        {
	  typeset_element (volta_p_arr_[i]);
	  volta_p_arr_[i] = 0;
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
#if 0
  Time_description const *time = get_staff_info().time_C_;
  Moment now = now_moment ();
  for (int i = volta_p_arr_.size (); i--; )
    {
      if ((now > alternative_stop_mom_arr_[i])
	  && !time->whole_in_measure_)
        {
	  volta_p_arr_[i] = 0;
	  volta_p_arr_.del (i);
	  alternative_music_arr_[i] = 0;
	  alternative_music_arr_.del (i);
	  alternative_start_mom_arr_.del (i);
	  alternative_stop_mom_arr_.del (i);
	}
    }
#endif
}

