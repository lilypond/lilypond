/*
  bar-engraver.hh -- declare Bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BAR_ENGRAVER_HH
#define BAR_ENGRAVER_HH

#include "engraver.hh"

/**
  generate bars. Either user ("|:"), or default (new measure)
  */
class Bar_engraver : public Engraver
{
public:
  Bar_engraver();
  VIRTUAL_COPY_CONS(Translator);
  
  void request_bar (String type_str);
    
protected:
  virtual void do_creation_processing ();
  virtual void do_removal_processing ();
  virtual void do_process_music();
  virtual void do_pre_move_processing();


private:
  void create_bar ();

  Item * bar_p_;
};

#endif // BAR_ENGRAVER_HH
