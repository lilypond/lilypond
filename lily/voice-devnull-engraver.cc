/*
  voice-devnull-engraver.cc -- implement Voice_devnull_engraver

  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
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
  virtual bool try_music (Music *m);
  virtual void acknowledge_grob (Grob_info);
};

ADD_THIS_TRANSLATOR (Voice_devnull_engraver);

static char const *eat_spanners[] = {
  "beam",
  "crescendo",
  "decrescendo",
  "slur",
  0
};

bool
Voice_devnull_engraver::try_music (Music *m)
{
  SCM s = get_property ("devNullVoice");
#if 0
  /* No need */
  if (gh_equal_p (s, ly_symbol2scm ("never")))
    return;
#endif

  if (gh_equal_p (s, ly_symbol2scm ("allways"))
      || (s == SCM_EOL
	  && daddy_trans_l_->id_str_.left_str (3) == "two"
	  && (to_boolean (get_property ("unison"))
	      || to_boolean (get_property ("unisilence")))))
    {
      if (Span_req *s = dynamic_cast <Span_req *> (m))
	{
	  SCM t = s->get_mus_property ("span-type");
        
	  for (char const **p = eat_spanners; *p; p++)
	    {
	      if (t == ly_str02scm (*p))
		return true;
	    }
	}
      /* Ugh.  Should eat other requests, script etc. too. */  
      else if (Tie_req *t = dynamic_cast<Tie_req*> (m))
	return true;
    }
  return false;
}
    
static char const *junk_interfaces[] = {
  "beam-interface",
  "dynamic-interface",
  "hairpin-interface",
  "multi-measure-rest-interface",
  "script-interface",
  "slur-interface",
  "text-interface",
  "text-item-interface",
  "text-script-interface",
  "text-spanner-interface",
  "tie-interface",
  0
};

void
Voice_devnull_engraver::acknowledge_grob (Grob_info i)
{
  SCM s = get_property ("devNullVoice");
#if 0
  /* No need, next if will never be true */
  if (s == ly_symbol2scm ("never"))
    return;
#endif

  if (s == ly_symbol2scm ("allways")
      || (s == SCM_EOL
	  && daddy_trans_l_->id_str_.left_str (3) == "two"
	  && (to_boolean (get_property ("unison"))
	      || to_boolean (get_property ("unisilence")))))
    for (char const **p = junk_interfaces; *p; p++)
      if (i.elem_l_->has_interface (ly_symbol2scm (*p)))
	{
#if 0
	  /* Ugh: virtual mmrest::suicide () ? */
	  if (i.elem_l_->has_interface (ly_symbol2scm ("multi-measure-rest-interface")))
	    i.elem_l_->set_grob_property ("skip-timestep", SCM_BOOL_T);
	  else
	    ;
#endif	  
	  /* Ugh, we can suicide them, but they remain living */
	  i.elem_l_->suicide ();
	  return;
	}
}
 
  
