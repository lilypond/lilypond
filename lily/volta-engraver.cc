/*   
  volta-engraver.cc --  implement Volta_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "translator-group.hh"
#include "volta-spanner.hh"
#include "item.hh"
#include "note-column.hh"
#include "bar.hh"
#include "side-position-interface.hh"

/*
  Create Volta spanners, by reading repeatCommands  property, usually
  set by Unfolded_repeat_iterator.
 */
class Volta_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Volta_engraver);
protected:

  virtual void acknowledge_grob (Grob_info);
  virtual void finalize ();
  virtual void stop_translation_timestep ();
  virtual void process_music ();
  virtual void create_grobs ();
  
  Moment started_mom_;
  Spanner *volta_span_p_;
  Spanner *end_volta_span_p_;

  SCM start_str_;
};

Volta_engraver::Volta_engraver ()
{
  volta_span_p_ = 0;
  end_volta_span_p_ = 0;
}


void
Volta_engraver::process_music ()
{
  SCM cs = get_property ("repeatCommands");

  bool  end = false;
  start_str_ = SCM_EOL;
  while (gh_pair_p (cs))
    {
      SCM c = ly_car (cs);

      if (gh_pair_p (c) && ly_car (c) == ly_symbol2scm ("volta")
	  && gh_pair_p (ly_cdr (c)))
	{
	  if (ly_cadr (c) ==  SCM_BOOL_F)
	    end = true;
	  else
	    start_str_ = ly_cadr (c);
	}
      
      cs = ly_cdr (cs);
    }

  if (volta_span_p_)
    {
      SCM l (get_property ("voltaSpannerDuration"));
      Moment now = now_mom ();
  
      bool early_stop = unsmob_moment (l)
	&& *unsmob_moment (l) <= now - started_mom_;
      
      end = end || early_stop;
    }

  
  if (end && !volta_span_p_)
    {
      warning (_ ("No volta spanner to end")); // fixme: be more verbose.
    }
  else if (end)
    {
      end_volta_span_p_ = volta_span_p_;
      volta_span_p_ =0;

      /*
	maybe do typeset_grob () directly?
      */

      if (!gh_string_p (start_str_))
	end_volta_span_p_->set_grob_property ("last-volta", SCM_BOOL_T);
    }

  if (gh_string_p (start_str_) && volta_span_p_)
    {
      warning (_ ("Already have a volta spanner.  Stopping that one prematurely."));
      
      if (end_volta_span_p_)
	{
	  warning (_ ("Also have a stopped spanner.  Giving up."));
	  return ;
	}

      end_volta_span_p_ = volta_span_p_;
      volta_span_p_ = 0;
    }
}

/*
  this could just as well be done in process_music (), but what the hack.
 */
void
Volta_engraver::create_grobs ()
{
  if (!volta_span_p_ && gh_string_p (start_str_))
    {
      started_mom_ = now_mom () ;

      volta_span_p_ = new Spanner (get_property ("VoltaBracket"));
      Volta_spanner::set_interface (volta_span_p_);
      announce_grob (volta_span_p_,0);
      volta_span_p_->set_grob_property ("text", start_str_);
    }
}

void
Volta_engraver::acknowledge_grob (Grob_info i)
{
  if (Item* item = dynamic_cast<Item*> (i.grob_l_))
    {
      if (Note_column::has_interface (item))
	{
	  if (volta_span_p_)
	    Volta_spanner::add_column (volta_span_p_,item);
	}
      if (Bar::has_interface (item))
	{
	  if (volta_span_p_)
	    Volta_spanner::add_bar (volta_span_p_, item);
	  if (end_volta_span_p_)
	    Volta_spanner::add_bar (end_volta_span_p_ , item);
	}
    }
}

void
Volta_engraver::finalize ()
{
  if (volta_span_p_)
    {
      typeset_grob (volta_span_p_);
    }
  if (end_volta_span_p_)
    {
      typeset_grob (end_volta_span_p_);
    }
}



void 
Volta_engraver::stop_translation_timestep ()
{
  if (end_volta_span_p_)
    {
      Side_position_interface::add_staff_support (end_volta_span_p_);
      
      typeset_grob (end_volta_span_p_);
      end_volta_span_p_ =0;
    }
}

/*
  TODO: should attach volta to paper-column if no bar is found.
 */

ENTER_DESCRIPTION(Volta_engraver,
/* descr */       "Make volta brackets",
/* creats*/       "VoltaBracket",
/* acks  */       "bar-line-interface note-column-interface",
/* reads */       "repeatCommands voltaSpannerDuration",
/* write */       "");
