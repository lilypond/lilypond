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
      Moment l = m->get_length ();

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

LY_DEFINE(ly_grob_pq_less_p, 
	  "ly-grob-pq-less?", 2 , 0 ,0, (SCM a, SCM b), 
	  "Compare 2 Grob PQ entries. Internal")
{
  if ( Moment::compare (*unsmob_moment (gh_car (a)),
				      *unsmob_moment (gh_car (b))) < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}
	  

void
Grob_pq_engraver::stop_translation_timestep ()
{
  Moment now = now_mom();

  current_grobs_.sort (&compare);
  SCM current_list = SCM_EOL;
  for (int i = current_grobs_.size(); i--;)
    current_list = scm_cons (scm_cons (current_grobs_[i].end_.smobbed_copy(), 
				       current_grobs_[i].grob_->self_scm ()), current_list);

  /*
    We generate some garbage here.
   */
  SCM busy = get_property ("busyGrobs");
  while (gh_pair_p (busy) && *unsmob_moment (gh_caar (busy)) == now)
    {
      busy = gh_cdr (busy);
    }
  
  busy = scm_merge_x (current_list, busy, ly_grob_pq_less_p_proc);
  current_grobs_.clear ();
  daddy_trans_->set_property ("busyGrobs", busy);
}

void
Grob_pq_engraver::start_translation_timestep ()
{
 Moment now = now_mom();

  SCM start_busy = get_property ("busyGrobs");
  SCM busy = start_busy;
  while (gh_pair_p (busy) && *unsmob_moment (gh_caar (busy)) < now)
    {
      /*
	Todo: do something sensible. The grob-pq-engraver is not water
	tight, and stuff like tupletSpannerDuration confuses it.
       */
      programming_error (_f("Skipped something?\nGrob %s ended before I expected it to end.", unsmob_grob (gh_cdar (busy))->name().to_str0()));
      
      busy = gh_cdr (busy);
    }

  if (start_busy != busy)
    daddy_trans_->set_property ("busyGrobs", busy);

}


ENTER_DESCRIPTION(Grob_pq_engraver,

/* descr */       "Administrate when certain grobs (eg. note heads) stop playing; this \
engraver is a sort-of a failure, since it doesn't handle all sorts of \
borderline cases very well. \
",												  \
		  
/* creats*/       "",										  \
/* accepts */     "",										  \
/* acks  */      "grob-interface",								  \
/* reads */       "busyGrobs",									  \
/* write */       "busyGrobs");
