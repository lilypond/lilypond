/*   
  grob-pq-engraver.cc --  implement Grob_pq_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2001--2002  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "engraver.hh"
#include "grob.hh"
#include "warn.hh"

struct Grob_mom
{
  Grob * grob_ ;
  Moment end_;
  Grob_mom () {}
  Grob_mom (Grob*gr, Moment e)
  {
    grob_ = gr;
    end_ = e;
  }
};

int compare  (Grob_mom const &a, Grob_mom const &b)
{
  return Moment::compare (a.end_, b.end_);
}

class Grob_pq_engraver: public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Grob_pq_engraver);

  Array<Grob_mom> current_grobs_;
protected:
  virtual void initialize ();
  virtual void acknowledge_grob (Grob_info);
  virtual void start_translation_timestep ();
  virtual void stop_translation_timestep ();
};


Grob_pq_engraver::Grob_pq_engraver()
{
}


void
Grob_pq_engraver::initialize ()
{
  daddy_trans_->set_property ("busyGrobs", SCM_EOL); 
}

void
Grob_pq_engraver::acknowledge_grob (Grob_info gi)
{
  Music  * m = gi.music_cause ();

  if (m)
    {
      Moment n = now_mom ();
      Moment l = m->length_mom ();

      if (!l.to_bool ())
	return ;
      
      if (n.grace_part_)
	{
	  l.grace_part_ = l.main_part_;
	  l.main_part_ = 0;
	}

      current_grobs_.push (Grob_mom (gi.grob_, n + l));
    }
}

void
Grob_pq_engraver::stop_translation_timestep ()
{
  Moment now = now_mom();

  current_grobs_.sort (&compare);

  SCM busy = get_property ("busyGrobs");
  while (gh_pair_p (busy) && *unsmob_moment (gh_caar (busy)) == now)
    {
      busy = gh_cdr (busy);
    }
  
  SCM start = busy;
  SCM * current_cell = &start;

  int  i = 0; 
  while (i  < current_grobs_.size ())
    {
      Moment stop;
      stop.set_infinite (1);
      
      if (gh_pair_p (busy))
	{
	  SCM h = gh_car (busy);
	  stop = *unsmob_moment (gh_car (h));
	}

      Moment current_stop = current_grobs_[i].end_;
      if (current_stop <= stop)
	{
	  SCM new_entry = gh_cons (current_stop.smobbed_copy(),
				   current_grobs_[i].grob_->self_scm ());

	  /*
	    Insert before BUSY.
	   */
	  i ++;
	  *current_cell = gh_cons (new_entry, busy);
	  current_cell = SCM_CDRLOC(*current_cell);
	}
      else
	{
	  /*
	    if current_stop > stop, then stop != infty, and we
	    apparently have a next entry */
	  busy = gh_cdr (busy);
	  current_cell = SCM_CDRLOC(*current_cell);
	}
    }

  current_grobs_.clear ();
  daddy_trans_->set_property ("busyGrobs", start);
}

void
Grob_pq_engraver::start_translation_timestep ()
{
 Moment now = now_mom();

  SCM start_busy = get_property ("busyGrobs");
  SCM busy = start_busy;
  while (gh_pair_p (busy) && *unsmob_moment (gh_caar (busy)) < now)
    {
      programming_error ("Skipped something ?!");
      
      busy = gh_cdr (busy);
    }

  if (start_busy != busy)
    daddy_trans_->set_property ("busyGrobs", busy);

}


ENTER_DESCRIPTION(Grob_pq_engraver,
/* descr */       "Administrate when certain grobs (eg. note heads) stop playing.
",
/* creats*/       "",
/* acks  */       "grob-interface",
/* reads */       "busyGrobs",
/* write */       "busyGrobs");
