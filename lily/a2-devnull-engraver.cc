/*
  a2-devnull-engraver.cc -- implement A2_devnull_engraver

  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "item.hh"
#include "musical-request.hh"
#include "translator-group.hh"

class A2_devnull_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  
protected:
  virtual void acknowledge_element (Score_element_info);
};

ADD_THIS_TRANSLATOR (A2_devnull_engraver);

void
A2_devnull_engraver::acknowledge_element (Score_element_info i)
{
  if (daddy_trans_l_->id_str_ == "two"
      && to_boolean (get_property ("unison"))
      && to_boolean (get_property ("soloADue")))
    i.elem_l_->suicide ();
}
