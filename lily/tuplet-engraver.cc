/*   
  auto-plet-engraver.cc --  implement Auto_plet_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "tuplet-engraver.hh"
#include "command-request.hh"
#include "tuplet-spanner.hh"
#include "note-column.hh"
#include "time-scaled-music.hh"
#include "beam.hh"
#include "music-list.hh"

bool
Tuplet_engraver::do_try_music (Music *r)
{
  if (Time_scaled_music * c = dynamic_cast<Time_scaled_music *> (r))
    {
      Music *el = c->element_l ();
      if (!dynamic_cast<Request_chord*> (el))
	{
	  time_scaled_music_arr_.push (c);
	  stop_moments_.push (now_mom () + c->length_mom ());
	}
      return true;
    }
  return false;
}

void
Tuplet_engraver::do_process_requests ()
{
  int dir = 0;
  SCM prop = get_property ("tupletDirection", 0);
  if (isdir_b (prop))
    dir = to_dir (prop);
  int visibility = 3;
  prop = get_property ("tupletVisibility", 0);
  if (gh_number_p(prop))
    visibility = gh_scm2int (prop);	// bool ?

  for (int i= started_span_p_arr_.size ();
       i < time_scaled_music_arr_.size (); i++)
    {
      Tuplet_spanner* glep = new Tuplet_spanner;
      started_span_p_arr_.push (glep);
      glep->set_elt_property ("text",
			      ly_str02scm (to_str (time_scaled_music_arr_[i]->den_i_).ch_C()));
      
      glep->set_elt_property("tuplet-visibility",
                             gh_int2scm (visibility));
      if (dir != 0)
	glep->set_elt_property("dir-forced", gh_int2scm (dir));
      announce_element (Score_element_info (glep, time_scaled_music_arr_ [i]));
    }
}

void
Tuplet_engraver::acknowledge_element (Score_element_info i)
{
  bool grace= to_boolean (i.elem_l_->get_elt_property ("grace"));
  SCM wg = get_property ("weAreGraceContext",0);
  bool wgb = to_boolean (wg);
  if (grace != wgb)
    return;
  
  if (Note_column *nc = dynamic_cast<Note_column *> (i.elem_l_))
    {
      for (int j =0; j  <started_span_p_arr_.size (); j++)
	started_span_p_arr_[j]->add_column (nc);
    }
  else if (Beam *b = dynamic_cast<Beam *> (i.elem_l_))
    {
      for (int j = 0; j < started_span_p_arr_.size (); j++)
	started_span_p_arr_[j]->add_beam (b);
    }
}

void
Tuplet_engraver::do_post_move_processing ()
{
  Moment now = now_mom ();
  for (int i= started_span_p_arr_.size (); i--; )
    {
      if (now >= stop_moments_[i])
	{
	  typeset_element (started_span_p_arr_[i]);
	  started_span_p_arr_.del (i);
	  stop_moments_.del(i);
	  time_scaled_music_arr_.del(i);
	}
    }
}

void
Tuplet_engraver::do_removal_processing ()
{
  for (int i=0; i < started_span_p_arr_.size (); i++)
    {
      typeset_element (started_span_p_arr_[i]);
    }  
}

ADD_THIS_TRANSLATOR(Tuplet_engraver);


