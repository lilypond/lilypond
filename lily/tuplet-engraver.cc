/*   
  auto-plet-engraver.cc --  implement Auto_plet_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "tuplet-engraver.hh"
#include "command-request.hh"
#include "tuplet-spanner.hh"
#include "note-column.hh"
#include "compressed-music.hh"
#include "text-def.hh"
#include "beam.hh"
#include "music-list.hh"

bool
Tuplet_engraver::do_try_music (Music *r)
{
  if (Compressed_music * c = dynamic_cast<Compressed_music *> (r))
    {
      Music *el = c->element_l ();
      if (!dynamic_cast<Request_chord*> (el))
	{
	  compressed_music_arr_.push (c);
	  stop_moments_.push (now_moment () + c->duration ());
	}
      return true;
    }
  return false;
}

void
Tuplet_engraver::do_process_requests ()
{
  for (int i= started_span_p_arr_.size ();
       i < compressed_music_arr_.size (); i++)
    {
      Tuplet_spanner* glep = new Tuplet_spanner;
      started_span_p_arr_.push (glep);

      Text_def *t = new Text_def;
      t->text_str_ = to_str (compressed_music_arr_[i]->den_i_);
      glep->tdef_p_.set_p  (t);
      announce_element (Score_element_info (glep, compressed_music_arr_ [i]));
    }
}

void
Tuplet_engraver::acknowledge_element (Score_element_info i)
{
  if (Note_column *nc = dynamic_cast<Note_column *> (i.elem_l_))
    {
      for (int j =0; j  <started_span_p_arr_.size (); j++)
	started_span_p_arr_[j]->add_column (nc);
    }
  else if (Beam *b = dynamic_cast<Beam *> (i.elem_l_))
    {
      for (int j = 0; j < started_span_p_arr_.size (); j++)
	started_span_p_arr_[j]->set_beam (b);
    }
}

void
Tuplet_engraver::do_post_move_processing ()
{
  Moment now = now_moment ();
  for (int i= started_span_p_arr_.size (); i--; )
    {
      if (now >= stop_moments_[i])
	{
	  typeset_element (started_span_p_arr_[i]);
	  started_span_p_arr_.del (i);
	  stop_moments_.del(i);
	  compressed_music_arr_.del(i);
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

