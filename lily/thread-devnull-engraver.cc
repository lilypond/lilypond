/*
  thread-devnull-engraver.cc -- implement Thread_devnull_engraver

  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
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
  SCM s = get_property ("devNullThread");
#if 0
  /* No need */
  if (gh_equal_p (s, ly_symbol2scm ("never")))
    return;
#endif

  if (gh_equal_p (s, ly_symbol2scm ("allways"))
      || (s == SCM_EOL
	  && daddy_trans_l_->id_str_.left_str (3) == "two"
	  && (to_boolean (get_property ("unison"))
	      || to_boolean (get_property ("unisilence")))
	  && to_boolean (get_property ("soloADue"))))
    i.elem_l_->suicide ();
}
