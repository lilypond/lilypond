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
  virtual bool do_try_music (Music *);
};

ADD_THIS_TRANSLATOR (A2_devnull_engraver);
/*
  Maybe this is too crude.
  It would probably be better to have a devnull engraver in StaffContext,
  that acknowledges stuff and suicides it.
 */
bool
A2_devnull_engraver::do_try_music (Music *m)
{
  if (1 //daddy_trans_l_->id_str_ == "two"
      && (to_boolean (get_property ("unison"))
	  || to_boolean (get_property ("unisilence"))))
    {
      /*
	We mustn't junk Rest requests
       */
      if (Span_req *s = dynamic_cast <Span_req *> (m))
	{
	  if (s->span_type_str_ == "slur"
	      || s->span_type_str_ == "beam"
	      || s->span_type_str_ == "crescendo"
	      || s->span_type_str_ == "decrescendo")
	    {
	      return true;
	    }
	  return false;
	}
      else if (Tie_req *t = dynamic_cast<Tie_req*> (m))
	{
	  return true;
	}
      else if (Text_script_req *d = dynamic_cast <Text_script_req*> (m))
	{
	  return true;
	}
    }
  return false;
}

void
A2_devnull_engraver::acknowledge_element (Score_element_info i)
{
  if (daddy_trans_l_->id_str_ == "two"
      && (to_boolean (get_property ("unison"))
	  || to_boolean (get_property ("unisilence")))
      && to_boolean (get_property ("soloADue")))
    i.elem_l_->suicide ();
}
