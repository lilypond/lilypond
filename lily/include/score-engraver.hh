/*
  score-engraver.hh -- declare Score_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCORE_GRAV_HH
#define SCORE_GRAV_HH

#include "engraver-group.hh"
#include "global-translator.hh"

/**
  Top level engraver. Puts elements into appropriate columns.
 */
class Score_engraver : 
  public Engraver_group_engraver, public Global_translator 
{
  Line_of_score * scoreline_l_;
  int break_penalty_i_;
  int breaks_i_;

  Link_array<Score_element> elem_p_arr_;
    
  Score_column* command_column_l_;
  Score_column* musical_column_l_;
    
  void set_columns (Score_column*,Score_column*);
  void typeset_all();
    
public:
  TRANSLATOR_CLONE(Score_engraver);
  Paper_score * pscore_p_;
  DECLARE_MY_RUNTIME_TYPEINFO;

  Score_engraver();
  virtual Music_output *get_output_p ();  
protected:   
  virtual void prepare (Moment);
  virtual void finish();
  virtual void process();
  virtual int depth_i() const { return Global_translator::depth_i ();}

protected:
  /* Engraver_group_engraver interface */
  virtual Staff_info get_staff_info() const;
  virtual bool do_try_request (Request*);
  virtual void do_creation_processing();
  virtual void do_removal_processing();
  virtual void announce_element (Score_element_info);
  virtual void do_announces();
  virtual void typeset_element (Score_element*elem_p);

  virtual void do_pre_move_processing();
  virtual void do_add_processing ();
};

#endif // SCORE_GRAV_HH
