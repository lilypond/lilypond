/*   
  plet-engraver.cc --  implement Tuplet_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "command-request.hh"
#include "tuplet-bracket.hh"
#include "note-column.hh"
#include "time-scaled-music.hh"
#include "beam.hh"
#include "music-list.hh"
#include "engraver.hh"
#include "spanner.hh"

class Tuplet_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Tuplet_engraver);

protected:
  Link_array<Time_scaled_music> time_scaled_musics_;
  /// when does the scaled music stop? Array order is synced with time_scaled_musics_
  Array<Rational> stop_moments_;
  /// when does the current spanner stop? Array order is synced with time_scaled_musics_
  Array<Rational> span_stop_moments_;
  
  /// The spanners. Array order is synced with time_scaled_musics_
  Link_array<Spanner> started_spanners_;

  virtual void finalize ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*r);
  virtual void start_translation_timestep ();
  virtual void process_acknowledged_grobs ();
};

bool
Tuplet_engraver::try_music (Music *r)
{
  if ( c->is_mus_type ("time-scaled-music"))
    {
      Music *el = c->element ();
      if (!dynamic_cast<Request_chord*> (el))
	{
	  time_scaled_musics_.push (c);
	  Rational m = now_mom ().main_part_ + c->length_mom ().main_part_;
	  stop_moments_.push (m);

	  SCM s = get_property ("tupletSpannerDuration");
	  if (unsmob_moment (s))
	    m = m <? (now_mom () + *unsmob_moment (s)).main_part_;
	  
	  span_stop_moments_.push (m);
	}
      return true;
    }
  return false;
}

void
Tuplet_engraver::process_acknowledged_grobs ()
{
  SCM v = get_property ("tupletInvisible");
  if (to_boolean (v))
    return;

  for (int i= 0; i < time_scaled_musics_.size (); i++)
    {
      if (i < started_spanners_.size () && started_spanners_[i])
	continue;

      Spanner* glep = new Spanner (get_property ("TupletBracket"));

      if (i >= started_spanners_.size ())
	started_spanners_.push (glep);
      else
	started_spanners_[i] = glep;
      

      SCM proc = get_property ("tupletNumberFormatFunction");
      if (gh_procedure_p (proc))
	{
	  SCM t = gh_apply (proc, scm_list_n (time_scaled_musics_[i]->self_scm (), SCM_UNDEFINED));
	  glep->set_grob_property ("text", t);
	}
      
      announce_grob(glep, time_scaled_musics_ [i]->self_scm());
    }
}

void
Tuplet_engraver::acknowledge_grob (Grob_info i)
{
  if (Note_column::has_interface (i.grob_))
    {
      for (int j =0; j  <started_spanners_.size (); j++)
	if (started_spanners_[j]) 
	  Tuplet_bracket::add_column (started_spanners_[j], dynamic_cast<Item*> (i.grob_));
    }
}

void
Tuplet_engraver::start_translation_timestep ()
{
  Moment now = now_mom ();

  Moment tsd;
  SCM s = get_property ("tupletSpannerDuration");
  if (unsmob_moment (s))
    tsd = unsmob_moment (s)->main_part_;

  for (int i= started_spanners_.size (); i--;)
    {
      if (now.main_part_ >= span_stop_moments_[i])
	{
	  if (started_spanners_[i])
	    {
	      typeset_grob (started_spanners_[i]);
	      started_spanners_[i] =0;
	    }
	  
	  if (tsd.to_bool ())
	    span_stop_moments_[i] += tsd.main_part_;
	}

      if (now.main_part_ >= stop_moments_[i])
	{
	  started_spanners_.del (i);
	  stop_moments_.del (i);
	  span_stop_moments_.del (i);
	  time_scaled_musics_.del (i);
	}
    }
}

void
Tuplet_engraver::finalize ()
{
  for (int i=0; i < started_spanners_.size (); i++)
    {
      if (started_spanners_[i])
	typeset_grob (started_spanners_[i]);
    }  
}



Tuplet_engraver::Tuplet_engraver(){}

ENTER_DESCRIPTION(Tuplet_engraver,
/* descr */       "Catch Time_scaled_music and generate appropriate bracket  ",
/* creats*/       "TupletBracket",
/* accepts */     "time-scaled-music",
/* acks  */      "note-column-interface",
/* reads */       "tupletNumberFormatFunction tupletSpannerDuration tupletInvisible",
/* write */       "");
