/*   
  bar-script-engraver.hh -- declare Bar_script_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
public:
  VIRTUAL_COPY_CONS(Translator);
protected:
  Text_item* text_p_;
  Protected_scm visibility_lambda_;
  String type_;
  Axis axis_;

protected:
  /**
    Put the script on #it#
   */
  void attach_script_to_item (Item *it);
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

