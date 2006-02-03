/*
  score-engraver.hh -- declare Score_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SCORE_ENGRAVER_HH
#define SCORE_ENGRAVER_HH

#include "engraver-group.hh"
#include "score-translator.hh"

class Score_engraver : public virtual Score_translator,
		       public virtual Engraver_group
{
  System *system_;

  Link_array__Grob_ elems_;
  Paper_score *pscore_;

  void typeset_all ();

protected:
  /* Score_translator */
  virtual void finish ();
  virtual void prepare (Moment);
  virtual void one_time_step ();

  /* Engraver_group_engraver interface */
  virtual bool try_music (Music *);
  virtual void initialize ();
  virtual void finalize ();
  virtual void announce_grob (Grob_info);
  void stop_translation_timestep ();

  /*
    Translator interface
  */
  virtual void derived_mark () const;

public:
  Score_engraver ();
  void forbid_breaks ();
  virtual SCM get_output ();
};

#endif /* SCORE_ENGRAVER_HH */
