/*   
  piano-pedal-engraver.cc --  implement Piano_pedal_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Jan Nieuwenhuizen <janneke@gnu.org>
  
  Chris Jackson <chris@fluffhouse.org.uk> - extended to support
  bracketed pedals.
  TODO: support for __| |__ or __| Ped  instead of  ___/\__ for pedal up-down
 */

#include "engraver.hh"
#include "musical-request.hh"
#include "grob.hh"
#include "item.hh"
#include "lily-guile.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"
#include "axis-group-interface.hh"
#include "translator-group.hh"
#include "directional-element-interface.hh"
#include "note-column.hh"

class Piano_pedal_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Piano_pedal_engraver);
  ~Piano_pedal_engraver ();
protected:
  virtual void initialize ();
  virtual void finalize ();
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();

private:
  struct Pedal_info
  {
    char const * name_;
    Span_req* start_req_l_;
    Drul_array<Span_req*> req_l_drul_;
    Item* item_p_;
    Spanner* bracket_p_;     // A single portion of a pedal bracket
    Spanner* finished_bracket_p_;
    Spanner* line_spanner_;  // This grob contains all the pedals of the same type on the same staff
    Spanner* finished_line_spanner_;
    Span_req* current_bracket_req_;
  };


  Pedal_info *info_list_;

  Spanner *previous_p_ [4]; // Record a stack of the current pedal spanners, so if more than one pedal
  int nspanners_i;          // occurs simultaneously then extra space can be added between them.
  void create_text_grobs (Pedal_info *p, SCM pedaltype);
  void create_bracket_grobs (Pedal_info *p, SCM pedaltype);
  void typeset_all();
};



Piano_pedal_engraver::Piano_pedal_engraver ()
{
  info_list_ = 0;
}
void
Piano_pedal_engraver::initialize ()
{
  info_list_ = new Pedal_info[4];
  Pedal_info *p = info_list_;

  nspanners_i = 0;
  for (int i = 0; i < 3; ++i)
    previous_p_[i] = 0; 


  char * names [] = { "Sostenuto", "Sustain", "UnaCorda", 0  };
  char **np = names ;
  do
    {
      p->name_ = *np;
      p->item_p_ = 0;
      p->bracket_p_ = 0;
      p->finished_bracket_p_ = 0;
      p->line_spanner_ = 0;
      p->finished_line_spanner_ = 0;
      p->current_bracket_req_ = 0;
      p->req_l_drul_[START] = 0;
      p->req_l_drul_[STOP] = 0;
      p->start_req_l_ = 0;

      p++;
    }
  while (* (np ++));
}

Piano_pedal_engraver::~Piano_pedal_engraver ()
{
  delete[] info_list_;
}

/*
   Urg: Code dup
   I'm a script
  */
void
Piano_pedal_engraver::acknowledge_grob (Grob_info info)
{
  for (Pedal_info*p = info_list_; p && p->name_; p ++)
    {
      Grob *g  = (Grob *) p->item_p_;
      int i = 0; 
      while ( i < 2 )
	{
	  if (g && p->line_spanner_) 
	    {
	      if (Note_column::has_interface (info.grob_l_))
		{
		  Side_position_interface::add_support (p->line_spanner_, info.grob_l_);
		  add_bound_item (p->line_spanner_,dynamic_cast<Item*> (info.grob_l_));
		  
		  if (Side_position_interface::get_axis (g) == X_AXIS
		      && !g->get_parent (Y_AXIS))
		    g->set_parent (info.grob_l_, Y_AXIS);
		}
	    }
	  g = (Grob *) p->bracket_p_;
	  ++i;
	}
    }
}

bool
Piano_pedal_engraver::try_music (Music *m)
{
  if (Span_req * s = dynamic_cast<Span_req*> (m))
    {
      for (Pedal_info*p = info_list_; p->name_; p ++)
	{
	  if (ly_scm2string (s->get_mus_property ("span-type")) == "abort")
	    {
	      p->req_l_drul_[START] = 0;
	      p->req_l_drul_[STOP] = 0;
	      
	      if (p->bracket_p_)
		p->bracket_p_->suicide (); /* as in dynamic-engraver.cc */
	      p->bracket_p_ = 0;
	    }  
	  if (scm_equal_p (s->get_mus_property ("span-type"),
			   ly_str02scm (p->name_))==SCM_BOOL_T)
	    {
	      p->req_l_drul_[s->get_span_dir ()] = s;
	      return true;
	    }
	}
    }
  return false;
}

void
Piano_pedal_engraver::create_grobs ()
{
  for (Pedal_info*p = info_list_; p && p->name_; p ++)
    {
      if (p->req_l_drul_[STOP] || p->req_l_drul_[START])
	{
	  if (!p->line_spanner_)
	    {
	      p->line_spanner_ = new Spanner (get_property ( ( String (p->name_) + "PedalLineSpanner").ch_C() ));
	      Side_position_interface::set_axis (p->line_spanner_, Y_AXIS);
	      Axis_group_interface::set_interface (p->line_spanner_);
	      Axis_group_interface::set_axes (p->line_spanner_, Y_AXIS, Y_AXIS);
	      Music * rq = (p->req_l_drul_[START]  ?  p->req_l_drul_[START]  :  p->req_l_drul_[STOP]);
	      announce_grob (p->line_spanner_, rq->self_scm ());
	    }
      
	  // Choose the appropriate grobs to add to the line spanner
	  // These can be text items or text-spanners
	  
	  SCM type = ly_cdr (scm_assoc (ly_symbol2scm("pedal-type"), 
					get_property( ( String (p->name_) + "Pedal").ch_C () )));
	  if (type == ly_symbol2scm("text") ||      // Ped.     *Ped.  *
	      type == ly_symbol2scm("mixed")   )    // Ped. _____/\____|
	    if (! p->item_p_)
	      create_text_grobs(p, type);

	  if (type == ly_symbol2scm("bracket") ||   // |_________/\____|
	      type == ly_symbol2scm("mixed")   )
	    create_bracket_grobs(p, type);
	}
      
    }
}


void
Piano_pedal_engraver::create_text_grobs (Pedal_info *p, SCM pedaltype)
{
  SCM b;
  SCM s = SCM_EOL;
  SCM strings = get_property (("pedal" + String (p->name_) + "Strings").ch_C ());

  if (! (scm_ilength (strings) < 3))
    {
      if (p->req_l_drul_[STOP] && p->req_l_drul_[START]) 
	{
	  if (pedaltype == ly_symbol2scm("text")) 
	    {
	      if (!p->start_req_l_)
		{
		  p->req_l_drul_[STOP]->origin ()->warning (_f ("can't find start of piano pedal: `%s'",  p->name_));
		}
	      else
		{
		  s = ly_cadr (strings);
		}
	      p->start_req_l_ = p->req_l_drul_[START];
	    }
	}
      
      else if (p->req_l_drul_[STOP])
	{ 
	  if (pedaltype == ly_symbol2scm("text"))
	    {
	      if (!p->start_req_l_)
		{
		  p->req_l_drul_[STOP]->origin ()->warning (_f ("can't find start of piano pedal: `%s'", p->name_));
		}
	      else
		{
		  s = ly_caddr (strings);
		  nspanners_i --;
		}
	      p->start_req_l_ = 0;
	    }
	}
      
      else if ( p->req_l_drul_[START] )
	{
	  p->start_req_l_ = p->req_l_drul_[START];
	  s = ly_car (strings);
	  if (pedaltype == ly_symbol2scm("text")) {
	    nspanners_i ++;
	    previous_p_[nspanners_i] = p->line_spanner_;
	    if  (nspanners_i > 1)
	      // add extra space below the previous already-occuring pedal
	      Side_position_interface::add_support(p->line_spanner_, previous_p_[nspanners_i - 1]);
	  }
	}
      
      if (gh_string_p (s))
	{
	  String propname = String (p->name_) + "Pedal";
	  b = get_property (propname.ch_C ());
	  p->item_p_ = new Item (b);
	  p->item_p_->set_grob_property ("text", s);
	  Axis_group_interface::add_element (p->line_spanner_, p->item_p_);
	  
	  announce_grob (p->item_p_,
			 (p->req_l_drul_[START]
			 ? p->req_l_drul_[START]
			 : p->req_l_drul_[STOP])->self_scm() );
	  
	}
      if (pedaltype == ly_symbol2scm("text")) 
	{
	  p->req_l_drul_[START] = 0;
	  p->req_l_drul_[STOP] = 0;
	}
    }
}

void
Piano_pedal_engraver::create_bracket_grobs (Pedal_info *p, SCM pedaltype)
{

  if (p->req_l_drul_[STOP])
    {
      if (!p->start_req_l_)
	{
	  p->req_l_drul_[STOP]->origin ()->warning (_f ("can't find start of piano pedal: `%s'", p->name_));
	}
      else if (!p->req_l_drul_[START])
	nspanners_i -- ;

      assert (!p->finished_bracket_p_ && p->bracket_p_);

      p->bracket_p_->set_bound (RIGHT, unsmob_grob(get_property ("currentMusicalColumn")));

      // Set a property so that the molecule-creating function will know whether the right edge should be angled ___/
      p->bracket_p_->set_grob_property("angle-right", gh_bool2scm((bool) p->req_l_drul_[START]) );
      add_bound_item (p->line_spanner_, p->bracket_p_->get_bound (RIGHT));	  

      p->finished_bracket_p_ = p->bracket_p_;
      p->bracket_p_ = 0;
      p->current_bracket_req_ = 0;
      p->start_req_l_ = p->req_l_drul_[START];
    }

  if (p->req_l_drul_[START])
    {
      p->start_req_l_ = p->req_l_drul_[START];
      p->current_bracket_req_ = p->req_l_drul_[START];

      p->bracket_p_  = new Spanner (get_property ("PianoPedalBracket"));
      p->bracket_p_->set_interface (ly_symbol2scm ("piano-pedal-interface"));

      // Set a property so that the molecule-creating function will know whether the left edge should be angled \___
      p->bracket_p_->set_grob_property("angle-left", gh_bool2scm((bool) p->req_l_drul_[STOP]) );

      // Set this property for 'mixed style' pedals,    Ped._______/\ ,  
      // so the molecule function will shorten the ____ line by the length of the Ped. text. 
      p->bracket_p_->set_grob_property("text-start", 
				       pedaltype == ly_symbol2scm("mixed") ? 
				       gh_bool2scm((bool) ! p->req_l_drul_[STOP]) :
				       gh_bool2scm(false));
      if (p->item_p_)
	p->bracket_p_->set_parent (p->item_p_, Y_AXIS);
      
      Side_position_interface::set_axis (p->bracket_p_, Y_AXIS);
      Side_position_interface::set_direction (p->bracket_p_, UP);
      p->bracket_p_->set_bound (LEFT, unsmob_grob (get_property ("currentMusicalColumn")));
      Axis_group_interface::add_element (p->line_spanner_, p->bracket_p_);	      

      add_bound_item (p->line_spanner_, p->bracket_p_->get_bound (LEFT));
      announce_grob (p->bracket_p_, p->req_l_drul_[START]->self_scm());

      if (!p->req_l_drul_[STOP]) {

	nspanners_i ++;
	previous_p_[nspanners_i] = p->line_spanner_;	

	if (nspanners_i > 1) 
	  // position new pedal spanner below the current one
	  Side_position_interface::add_support(p->line_spanner_, previous_p_[nspanners_i - 1]);      
	
      }
    }

  p->req_l_drul_[START] = 0;
  p->req_l_drul_[STOP] = 0;
}

void
Piano_pedal_engraver::finalize ()
{  
  for (Pedal_info*p = info_list_; p && p->name_; p ++)
    {
      if (p->line_spanner_
	  && p->line_spanner_->immutable_property_alist_ == SCM_EOL)
	p->line_spanner_ = 0;
      if (p->line_spanner_)
	{
	  p->finished_line_spanner_ = p->line_spanner_;
	  typeset_all ();
	}
      if (p->bracket_p_
	  && p->bracket_p_->immutable_property_alist_ == SCM_EOL)
	p->bracket_p_ = 0;
      if (p->bracket_p_)
	{
	  p->current_bracket_req_->origin ()->warning (_ ("unterminated pedal bracket"));
	  p->bracket_p_->suicide ();
	  p->bracket_p_ = 0;
	}
    }
}

  
void
Piano_pedal_engraver::stop_translation_timestep ()
{
  for (Pedal_info*p = info_list_; p && p->name_; p ++)
    {
      p->finished_line_spanner_ = p->line_spanner_;
    }
  typeset_all ();
}


void
Piano_pedal_engraver::typeset_all ()
{


  Item * sustain = 0;
  for (Pedal_info*p = info_list_; p->name_; p ++)
    {
      if (p->finished_line_spanner_
	  && p->finished_line_spanner_->immutable_property_alist_ == SCM_EOL)
	p->finished_line_spanner_ = 0;
      if (p->finished_bracket_p_
	  && p->finished_bracket_p_->immutable_property_alist_ == SCM_EOL)
	p->finished_bracket_p_ = 0;
      if (p->name_ == String ("Sustain"))
	sustain = p->item_p_;

      if (p->item_p_)
	{
	  /*
	    Hmm.
	  */
	  if (p->name_ != String ("Sustain"))
	    {
	      if (sustain)
		{
		  Side_position_interface::add_support (p->item_p_,sustain);
		}
	    }
	  typeset_grob (p->item_p_);
	  p->item_p_ = 0;
	}
      
      if (p->finished_bracket_p_)
	{
	  if (!p->finished_bracket_p_->get_bound (RIGHT))
	    {
	      p->finished_bracket_p_->set_bound (RIGHT, unsmob_grob (get_property ("currentMusicalColumn")));

	      if (p->finished_line_spanner_)
		add_bound_item (p->finished_line_spanner_,
				p->finished_bracket_p_->get_bound (RIGHT));
	    }
	  typeset_grob (p->finished_bracket_p_);
	  p->finished_bracket_p_ =0;
	}

      if (p->finished_line_spanner_)
	{
	  Side_position_interface::add_staff_support (p->finished_line_spanner_);
	  Grob * l = p->finished_line_spanner_->get_bound (LEFT );
	  Grob * r = p->finished_line_spanner_->get_bound (RIGHT);      
	  if (!r && l)
	    p->finished_line_spanner_->set_bound (RIGHT, l);
	  else if (!l && r)
	    p->finished_line_spanner_->set_bound (LEFT, r);
	  else if (!r && !l)
	    {
	      Grob * cc = unsmob_grob (get_property ("currentMusicalColumn"));
	      Item * ci = dynamic_cast<Item*>(cc);
	      p->finished_line_spanner_->set_bound (RIGHT, ci);
	      p->finished_line_spanner_->set_bound (LEFT, ci);	  
	    }
	  typeset_grob (p->finished_line_spanner_);
	  p->finished_line_spanner_ = 0;
	}
    }
}

void
Piano_pedal_engraver::start_translation_timestep ()
{
  for (Pedal_info*p = info_list_; p->name_; p ++)
    {
      p->req_l_drul_[STOP] = 0;
      p->req_l_drul_[START] = 0;
    }
}
ENTER_DESCRIPTION(Piano_pedal_engraver,
/* descr */       "Engrave piano pedal symbols and brackets.",
/* creats*/       "SostenutoPedal SustainPedal UnaCordaPedal SostenutoPedalLineSpanner SustainPedalLineSpanner UnaCordaPedalLineSpanner",
/* acks  */       "note-column-interface",
/* reads */       "pedalSostenutoStrings pedalSustainStrings pedalUnaCordaStrings",
/* write */       "");
