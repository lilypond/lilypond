/*
  clef-engraver.hh -- declare Clef_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef CLEF_GRAV_HH
#define CLEF_GRAV_HH

#include "scalar.hh"
#include "array.hh"
#include "engraver.hh"
#include "direction.hh"

/// where is c-0 in the staff?
class Clef_engraver : public  Engraver {
  Clef_item *clef_p_;
  Clef_change_req * clef_req_l_;
  void create_clef();
  void read_req (Clef_change_req*);
  bool set_type (String);
protected:
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
  virtual void do_removal_processing();
  virtual void do_creation_processing();
  virtual void do_post_move_processing();
  virtual bool do_try_request (Request*);
  virtual void acknowledge_element (Score_element_info);
public:
  TRANSLATOR_CLONE(Clef_engraver);
  int c0_position_i_;
  int clef_position_i_;
  Direction octave_dir_;
  String clef_type_str_;

   
  Clef_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
   
};
#endif 
