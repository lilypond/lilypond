/*
  repeat-engraver.cc -- implement Repeat_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "repeat-engraver.hh"
#include "bar.hh"
#include "bar-engraver.hh"
#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "command-request.hh"
#include "time-description.hh"
#include "engraver-group.hh"
#include "new-repeated-music.hh"
#include "time-description.hh"
#include "volta-spanner.hh"
#include "note-column.hh"
#include "paper-def.hh"
#include "music-list.hh"

ADD_THIS_TRANSLATOR (Repeat_engraver);

/*
  Urg. Hairy.  Needs redesign?
 */
bool
Repeat_engraver::do_try_music (Music* m)
{
  if (New_repeated_music* r = dynamic_cast<New_repeated_music *> (m))
    {
      Music_sequence* alt = r->alternatives_p_;
      Moment repeat_length_mom = r->repeat_body_p_->length_mom ();
      Moment stop_mom = now_mom () + repeat_length_mom;
      Moment alt_mom = now_mom () + repeat_length_mom;
      if (repeat_length_mom)
	{
	  stop_mom += r->alternatives_length_mom ();
	  repeated_music_arr_.push (r);
	  stop_mom_arr_.push (stop_mom);
	}

      /* 
	 Counting nested repeats, it seems safest to forbid
	 two pieces of alternative music to start at the same time.
      */
      for (int i = 0; i < alternative_start_mom_arr_.size (); i++)
        if (alternative_start_mom_arr_[i] == alt_mom)
	  return false;

      /*
	Coda kludge: see input/test/coda-kludge.ly
       */
      Moment span_mom;
      Scalar prop = get_property ("voltaSpannerDuration", 0);
      if (prop.length_i ())
	span_mom = prop.to_rat ();

      int alt_i = r->repeats_i_ + 1 - cons_list_size_i (alt->music_p_list_p_->head_ ) >? 1;
      for (Cons<Music> *i = alt->music_p_list_p_->head_; i ; i = i->next_)
        {
	  alternative_music_arr_.push (i->car_);
	  alternative_start_mom_arr_.push (alt_mom);
	  if (span_mom)
	    alternative_stop_mom_arr_.push (alt_mom + span_mom);
	  else
	    alternative_stop_mom_arr_.push (alt_mom + i->car_->length_mom ());
	  String str;
	  if ((alt_i != 1) && (alt_i != r->repeats_i_) && (i == alt->music_p_list_p_->head_))
	    str = "1.-";
	  str += to_str (alt_i) + ".";
	  alt_i++;
	  alternative_str_arr_.push (str);
	  if (!dynamic_cast<Simultaneous_music *> (alt))
	    alt_mom += i->car_->length_mom ();
	}
      return true;
    }
  return false;
}

void
Repeat_engraver::acknowledge_element (Score_element_info i)
{
  Moment now = now_mom ();
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
  Moment now = now_mom ();
  Bar_engraver* bar_engraver_l = dynamic_cast <Bar_engraver*>
    (daddy_grav_l ()->get_simple_translator ("Bar_engraver"));
  for (int i = bar_b_arr_.size (); i < repeated_music_arr_.size (); i++)
    {
      if (bar_engraver_l)
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
  for (int i = volta_p_arr_.size (); i < alternative_music_arr_.size (); i++)
    {
      Volta_spanner* v = new Volta_spanner;
      Scalar prop = get_property ("voltaVisibility", 0);
      if (!prop.to_bool ())
	v->set_elt_property (transparent_scm_sym, SCM_BOOL_T);
      prop = get_property ("voltaSpannerDuration", 0);
      if ((i == alternative_music_arr_.size () - 1) || prop.length_i ())
        v->last_b_ = true;

      v->number_str_ = alternative_str_arr_[i];
      volta_p_arr_.push (v);
      announce_element (Score_element_info (v, alternative_music_arr_[i]));
    }
}

void 
Repeat_engraver::do_pre_move_processing ()
{
  Moment now = now_mom ();
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
	  alternative_str_arr_.del (i);
	}
    }
}
