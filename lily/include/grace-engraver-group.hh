/*   
  grace-engraver-group.hh -- declare Grace_engraver_group
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef GRACE_ENGRAVER_GROUP_HH
#define GRACE_ENGRAVER_GROUP_HH

#include "engraver-group-engraver.hh"
#include "global-translator.hh"

class Grace_engraver_group : public Engraver_group_engraver, public Global_translator
{
  Link_array<Score_element> typeset_us_;
  Array<Score_element_info> announce_to_top_;
  bool calling_self_b_;
  bool pass_to_top_b (Music *) const;
public:
  VIRTUAL_COPY_CONS(Translator);
  Grace_engraver_group ();
protected:
  virtual void announce_element (Score_element_info);
  virtual void start ();
  virtual void finish ();
  virtual void process ();
  virtual void each (Method_pointer);
  virtual void do_removal_processing () ;
  virtual void typeset_element (Score_element*);
  virtual bool do_try_music (Music *m);
};


#endif /* GRACE_ENGRAVER_GROUP_HH */
