/*
  thread-devnull-engraver.cc -- implement Thread_devnull_engraver

  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "item.hh"
#include "musical-request.hh"
#include "translator-group.hh"

class Thread_devnull_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  
protected:
  virtual void acknowledge_grob (Grob_info);
};

ADD_THIS_TRANSLATOR (Thread_devnull_engraver);

void
Thread_devnull_engraver::acknowledge_grob (Grob_info i)
{
  /*
    FIXME: coriolan-fix hack -- ff  geen inspiratie
    
    We should have at least three modes:
    
      1. Never -- do nothing
      2. Allways -- junk everythingallways
      3. Regular: junk according to two/unison/unisilence/soloADue

    but how shall we do that?

    Hmm, maybe just: threadDevNull = 'never|'allways|...
   */
  if (!to_boolean (get_property ("disableDevNullThread"))
      && daddy_trans_l_->id_str_.left_str (3) == "two"
      && (to_boolean (get_property ("unison"))
	  || to_boolean (get_property ("unisilence")))
      && to_boolean (get_property ("soloADue")))
    i.elem_l_->suicide ();
}
