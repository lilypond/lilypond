/*
  score-engraver.hh -- declare Score_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCORE_ENGRAVER_HH
#define SCORE_ENGRAVER_HH

#include "engraver-group-engraver.hh"
#include "global-translator.hh"

/**
  Top level engraver. Puts elements into appropriate columns.
 */
class Score_engraver : 
  public Engraver_group_engraver, public Global_translator 
{
  System *system_;
  int breaks_;			// used for stat printing
  
  Link_array<Grob> elems_;
  Paper_column* command_column_;
  Paper_column* musical_column_;
  Paper_score * pscore_;

  void make_columns ();
  void set_columns (Paper_column*,Paper_column*);
  void typeset_all ();
protected:
  /* Global_translator */
  virtual void prepare (Moment);
  virtual void finish ();
  virtual void one_time_step ();

  /* Engraver_group_engraver interface */
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*);
  virtual void initialize ();
  virtual void finalize ();
  virtual void announce_grob (Grob_info);
  virtual void typeset_grob (Grob*elem);
  virtual void stop_translation_timestep ();

public:
  TRANSLATOR_DECLARATIONS(Score_engraver);
  void forbid_breaks ();
  virtual Music_output *get_output ();  
};

#endif /* SCORE_ENGRAVER_HH */
