/*
  voice-devnull-engraver.cc -- implement Voice_devnull_engraver

  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "item.hh"
#include "musical-request.hh"
#include "translator-group.hh"

class Voice_devnull_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  
protected:
  virtual void acknowledge_element (Score_element_info);
};

ADD_THIS_TRANSLATOR (Voice_devnull_engraver);

static char const *junk_interfaces[] = {
  //	"beam-interface",
  "slur-interface",
  "tie-interface",
  "text-item-interface",
  "text-script-interface",
  "dynamic-interface",
  "crescendo-interface",
  0
};

void
Voice_devnull_engraver::acknowledge_element (Score_element_info i)
{
  if (daddy_trans_l_->id_str_ == "two"
      && (to_boolean (get_property ("unison"))
	  || to_boolean (get_property ("unisilence"))))
    for (char const **p = junk_interfaces; *p; p++)
      if (i.elem_l_->has_interface (ly_symbol2scm (*p)))
	{
	  i.elem_l_->suicide ();
	  return;
	}
}
