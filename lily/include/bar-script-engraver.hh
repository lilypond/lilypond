/*   
  bar-script-engraver.hh -- declare Bar_script_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef BAR_SCRIPT_ENGRAVER_HH
#define BAR_SCRIPT_ENGRAVER_HH
#include "engraver.hh"
#include "protected-scm.hh"

/**
  put stuff over or next to  bars
 */
class Bar_script_engraver : public Engraver
{
protected:
  G_staff_side_item* staff_side_p_;
  G_text_item* text_p_;
  Protected_scm visibility_lambda_;
  String type_;
  Axis axis_;

protected:
  Bar_script_engraver ();
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info);
  void create_items(Request*);
};


#endif /* BAR_SCRIPT_ENGRAVER_HH */

