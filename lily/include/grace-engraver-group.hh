/*   
  grace-engraver-group.hh -- declare Grace_engraver_group
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef GRACE_ENGRAVER_GROUP_HH
#define GRACE_ENGRAVER_GROUP_HH

#include "engraver-group-engraver.hh"
#include "global-translator.hh"

class Grace_engraver_group : public Engraver_group_engraver, public Global_translator
{
  Link_array<Grob> typeset_us_;
  Array<Grob_info> announce_to_top_;
  bool calling_self_b_;
  bool pass_to_top_b (Music *) const;
public:
  VIRTUAL_COPY_CONS (Translator);
  Grace_engraver_group ();
protected:
  virtual void initialize ();
  virtual void announce_grob (Grob_info);
  virtual void start ();
  virtual void finish ();
  virtual void one_time_step ();
  virtual void each (Method_pointer);
  virtual void finalize () ;
  virtual void typeset_grob (Grob*);
  virtual bool try_music (Music *m);
};


#endif /* GRACE_ENGRAVER_GROUP_HH */
