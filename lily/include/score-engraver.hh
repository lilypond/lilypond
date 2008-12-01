/*
  score-engraver.hh -- declare Score_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SCORE_ENGRAVER_HH
#define SCORE_ENGRAVER_HH

#include "engraver-group.hh"

class Score_engraver : public Engraver_group
{
  System *system_;

  vector<Grob*> elems_;
  Paper_score *pscore_;

  void typeset_all ();

protected:
  DECLARE_LISTENER (finish);
  DECLARE_LISTENER (prepare);
  DECLARE_LISTENER (one_time_step);

  /* Engraver_group_engraver interface */
  virtual void connect_to_context (Context *);
  virtual void disconnect_from_context ();
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
};

#endif /* SCORE_ENGRAVER_HH */
