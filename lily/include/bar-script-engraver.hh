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
  put stuff over or next to  bars.  Examples: bar numbers, marginal notes,
  rehearsal marks.
 */
class Bar_script_engraver : public Engraver
{
protected:
  G_staff_side_item* staff_side_p_;
  G_text_item* text_p_;
  Protected_scm visibility_lambda_;
  String type_;
  Axis axis_;
  bool hang_on_clef_b_;
protected:
  /**
    Put the script on #it#
   */
  void do_acknowledge_element (Item *it);
  /**
     Return non-nil if we want to hang something on this.
   */
  Item *cast_to_interesting_item (Score_element*);
  Bar_script_engraver ();
  virtual void do_creation_processing ();
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info);
  void create_items(Request*);
};


#endif /* BAR_SCRIPT_ENGRAVER_HH */

