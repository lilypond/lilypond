/*   
  piano-pedal-engraver.cc --  implement Piano_pedal_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "engraver.hh"
#include "musical-request.hh"
#include "score-element.hh"
#include "item.hh"
#include "lookup.hh"
#include "lily-guile.hh"
#include "note-head.hh"
#include "stem.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"


/*
   TODO:
     * it would be really cool if an engraver could be initialised with a
       string, ie:

          Piano_pedal_engraver::"sostenuto"
          Piano_pedal_engraver::"sustain"
          Piano_pedal_engraver::"una-chorda"

 */

/*
  Would it? The semantics are unclear, and real benefits are muddy
  too.  --hwn
*/



/**
   engrave Piano pedals symbols.
 */
class Piano_pedal_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Piano_pedal_engraver ();
  ~Piano_pedal_engraver ();
protected:
  virtual void do_creation_processing ();
  virtual bool do_try_music (Music*);
  virtual void do_process_music ();
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void acknowledge_element (Score_element_info);

private:
  struct Pedal_info
  {
    char const * name_;
    Span_req* start_req_l_;
    Drul_array<Span_req*> req_l_drul_;
    Item* item_p_;
  };


  Pedal_info *info_list_;
};

ADD_THIS_TRANSLATOR (Piano_pedal_engraver);

Piano_pedal_engraver::Piano_pedal_engraver ()
{
  info_list_ = 0;
}
void
Piano_pedal_engraver::do_creation_processing()
{
  info_list_ = new Pedal_info[4];
  Pedal_info *p = info_list_;


  char * names [] = { "Sostenuto", "Sustain", "UnaChorda", 0  };
  char **np = names ;
  do
    {
      p->name_ = *np;
      p->item_p_ = 0;
      p->req_l_drul_[START] = 0;
      p->req_l_drul_[STOP] = 0;
      p->start_req_l_ = 0;

      p++;
    }
  while (*(np ++));
}

Piano_pedal_engraver::~Piano_pedal_engraver()
{
  delete[] info_list_;
}

/*
  Urg: Code dup
  I'm a script
 */
void
Piano_pedal_engraver::acknowledge_element (Score_element_info info)
{
  for (Pedal_info*p = info_list_; p->name_; p ++)
    {
      if (p->item_p_)
	{
	  if (Note_head* n = dynamic_cast<Note_head*> (info.elem_l_))
	    {
	      Side_position_interface st (p->item_p_);
	      st.add_support (n);
	      if (st.get_axis( ) == X_AXIS
		  && !p->item_p_->parent_l (Y_AXIS))
		p->item_p_->set_parent (n, Y_AXIS);
	    }
	  if (Stem* s = dynamic_cast<Stem*> (info.elem_l_))
	    {
	      Side_position_interface st (p->item_p_);
	      st.add_support (s);
	    }
	}
    }
}

bool
Piano_pedal_engraver::do_try_music (Music *m)
{
  if (Span_req * s = dynamic_cast<Span_req*>(m))
    {
      for (Pedal_info*p = info_list_; p->name_; p ++)
	{
	  if (s->span_type_str_ == p->name_)
	    {
	      p->req_l_drul_[s->span_dir_] = s;
	      return true;
	    }
	}
    }
  return false;
}

void
Piano_pedal_engraver::do_process_music ()
{
  for (Pedal_info*p = info_list_; p->name_; p ++)
    {
      SCM s = SCM_UNDEFINED;
      if (p->req_l_drul_[STOP] && p->req_l_drul_[START])
	{
	  if (!p->start_req_l_)
	    {
	      p->req_l_drul_[STOP]->warning (_f ("can't find start of piano pedal: %s",  p->name_ ));
	    }
	  else
	    {
	      s = get_property (("stopStart" + String (p->name_ )).ch_C());
	    }
	  p->start_req_l_ = p->req_l_drul_[START];
	}
      else if (p->req_l_drul_[STOP])
	{
	  if (!p->start_req_l_)
	    {
	      p->req_l_drul_[STOP]->warning (_f ("can't find start of piano pedal: %s", p->name_ ));
	    }
	  else
	    {
	      s = get_property (("stop" + String (p->name_ )).ch_C());
	    }
	  p->start_req_l_ = 0;
	}
      else if (p->req_l_drul_[START])
	{
	  p->start_req_l_ = p->req_l_drul_[START];
	  s = get_property (("start" + String (p->name_ )).ch_C());
	}

      if (gh_string_p (s))
	{
	  if (p->name_ == String ("Sustain"))
	    {
	      // fixme: Item should be sufficient.
	      p->item_p_ = new Item (get_property ("basicSustainPedalProperties"));
	    }
	  else
	    {
	      p->item_p_ = new Item (get_property ("basicPedalProperties"));
	    }
	  p->item_p_->set_elt_property ("text", s);
	  // guh

	  Side_position_interface si (p->item_p_);
	  si.set_axis (Y_AXIS);

	  // todo: init with basic props.
	  p->item_p_->add_offset_callback (Side_position_interface::aligned_on_self, X_AXIS);
	  p->item_p_->add_offset_callback (Side_position_interface::centered_on_parent, X_AXIS);
	  announce_element (Score_element_info (p->item_p_,
						p->req_l_drul_[START]
						? p->req_l_drul_[START]
						: p->req_l_drul_[STOP]));
	}
    }
}

void
Piano_pedal_engraver::do_pre_move_processing ()
{
  Item * sustain = 0;
  for (Pedal_info*p = info_list_; p->name_; p ++)
    {
      if (p->name_ == String ("Sustain"))
	sustain = p->item_p_;
    }

  for (Pedal_info*p = info_list_; p->name_; p ++)
    {
      if (p->item_p_)
	{
	  Side_position_interface (p->item_p_).add_staff_support ();
	  /*
	    Hmm.
	  */
	  if (p->name_ != String ("Sustain"))
	    {
	      if (sustain)
		{
		  Side_position_interface st (p->item_p_);
		  st.add_support (sustain);
		}
	    }
	  typeset_element (p->item_p_);
	}
      p->item_p_ = 0;
    }
}

void
Piano_pedal_engraver::do_post_move_processing ()
{
  for (Pedal_info*p = info_list_; p->name_; p ++)
    {
      p->req_l_drul_[STOP] = 0;
      p->req_l_drul_[START] = 0;
    }
}
