/*   
  quote-iterator.cc --  implement Quote_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "context.hh"
#include "event.hh"
#include "music-sequence.hh"
#include "lily-guile.hh"
#include "music-iterator.hh"
#include "music.hh"
#include "input.hh"
#include "warn.hh"


class Quote_iterator : public Music_iterator
{
public:
  Quote_iterator ();
  
  Moment start_moment_;
  SCM event_vector_;
  int event_idx_;
  int end_idx_ ;

  SCM transposed_musics_;
  
  DECLARE_SCHEME_CALLBACK (constructor, ()); 

  bool accept_music_type (Music*) const;
protected:
  virtual void derived_mark () const;
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void process (Moment);
  virtual bool ok () const;
};

bool
Quote_iterator::accept_music_type (Music *mus) const
{
  SCM accept = get_outlet()->get_property ("quotedEventTypes");
  for (SCM s =  mus->get_property ("types");
       scm_is_pair (s);  s = scm_cdr (s))
    {
      if (scm_memq (scm_car (s), accept) != SCM_BOOL_F)
	return true;
    }

  return false;
}


void
Quote_iterator::derived_mark () const
{
  scm_gc_mark (transposed_musics_ );
}

Quote_iterator::Quote_iterator ()
{
  transposed_musics_ = SCM_EOL;
  event_vector_ = SCM_EOL;
  event_idx_ = 0;
  end_idx_ = 0;
}

bool
moment_less (SCM a, SCM b)
{
  return  *unsmob_moment (a) < *unsmob_moment (b);
}


int
binsearch_scm_vector (SCM vec, SCM key, bool (*is_less)(SCM a,SCM b))
{
  int lo = 0;
  int hi = SCM_VECTOR_LENGTH (vec);

  /* binary search */
  do
  {
    int cmp = (lo + hi) / 2;

      SCM when = scm_caar (SCM_VECTOR_REF (vec, cmp));
      bool result =  (*is_less) (key, when);
      if (result)
          hi = cmp;
      else
          lo = cmp;
    }
  while (hi - lo > 1);

  return lo;
}


void
Quote_iterator::construct_children ()
{
  SCM dur = get_music ()->get_property ("duration");
  if (!unsmob_duration (dur))
    return ;

  set_translator (get_outlet ()->get_default_interpreter ());
  
  Moment now = get_outlet ()->now_mom ();
  Moment stop = now + unsmob_duration (dur)->get_length ();

  start_moment_ = now;
  event_vector_ = get_music ()->get_property ("quoted-events");

  if (ly_c_vector_p (event_vector_))
    {
      event_idx_ = binsearch_scm_vector (event_vector_, now.smobbed_copy (), &moment_less);
      end_idx_ = binsearch_scm_vector (event_vector_, stop.smobbed_copy (), &moment_less);
    }
  else
    {
      get_music ()->origin()->warning (_("No events found for \\quote"));
    }
}


bool
Quote_iterator::ok () const
{
  return ly_c_vector_p (event_vector_) && (event_idx_ <= end_idx_);
}

Moment
Quote_iterator::pending_moment () const
{
  SCM entry = SCM_VECTOR_REF (event_vector_, event_idx_);
  return *unsmob_moment (scm_caar (entry)) - start_moment_;
}

void
Quote_iterator::process (Moment m)
{
  SCM entry = SCM_EOL;

  m += start_moment_;
  while (event_idx_ < end_idx_)
    {
      entry = SCM_VECTOR_REF (event_vector_, event_idx_);

      Moment em = *unsmob_moment (scm_caar (entry));

      if (em > m)
	return ;

      if (em == m)
	break ;

      event_idx_++;
    }

  if (scm_is_pair (entry))
    {
      Pitch * quote_pitch = unsmob_pitch (scm_cdar (entry));
      Pitch * me_pitch = unsmob_pitch (get_outlet ()->get_property ("instrumentTransposition"));
      
      for (SCM s = scm_cdr (entry); scm_is_pair (s); s = scm_cdr (s))
	{
	  SCM ev_acc = scm_car (s);

	  Music * mus = unsmob_music (scm_car (ev_acc));
	  if (!mus)
	    programming_error ("need music in quote.");
	  else if (accept_music_type (mus))
	    {
	      if (quote_pitch || me_pitch)
		{
		  Pitch qp, mp;
		  if (quote_pitch)
		    qp = *quote_pitch;
		  if (me_pitch)
		    mp = *me_pitch;

		  Pitch diff = interval (mp, qp);

		  SCM copy = ly_deep_mus_copy (mus->self_scm ());
		  mus = unsmob_music (copy);
		  transposed_musics_ = scm_cons (copy, transposed_musics_);

		  
		  mus->transpose (diff);
		}

	      
	      bool b = get_outlet ()->try_music (mus);
      
	      if (!b)
		mus->origin ()->warning (_f ("In quotation: junking event %s", mus->name ()));
	    }
	}
    }
  event_idx_ ++; 
}

IMPLEMENT_CTOR_CALLBACK (Quote_iterator);
