/*   
  volta-engraver.cc --  implement Volta_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  Volta_engraver();
  VIRTUAL_COPY_CONS(Translator);
protected:

  virtual void start_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  virtual void finalize ();
  virtual void stop_translation_timestep ();
  void deprecated_process_music ();
  virtual void create_grobs ();
  
  Moment started_mom_;
  Spanner *volta_span_p_;
  Spanner *end_volta_span_p_;

  bool first_b_;
};

ADD_THIS_TRANSLATOR(Volta_engraver);

Volta_engraver::Volta_engraver ()
{
  first_b_ = true;
  volta_span_p_ = 0;
  end_volta_span_p_ = 0;
}

void
Volta_engraver::create_grobs ()
{
  if (first_b_)
    {
      deprecated_process_music ();
      first_b_ = false;
    }
}
  
void
Volta_engraver::deprecated_process_music ()
{
  if (volta_span_p_)
    return;
  SCM cs = get_property ("repeatCommands");

  SCM str = SCM_EOL; 
  bool end = false;
  while (gh_pair_p (cs))
    {
      SCM c = gh_car (cs);

      if (gh_pair_p (c) && gh_car (c) == ly_symbol2scm ("volta"))
	{
	  if (gh_cadr (c) ==  SCM_BOOL_F)
	    end = true;
	  else
	    str = gh_cadr (c);
	}
      
      cs = gh_cdr (cs);
    }

  SCM l (get_property ("voltaSpannerDuration"));
  Moment now = now_mom ();
  
  bool early_stop = volta_span_p_ &&    unsmob_moment (l)
    &&*unsmob_moment (l) <= now - started_mom_;

  if (end && !volta_span_p_)
    {
      warning (_("No volta spanner to end")); // fixme: be more verbose.
    }
  else if (end || early_stop)
    {
      end_volta_span_p_ = volta_span_p_;
      volta_span_p_ =0;

      /*
	maybe do typeset_grob () directly?
      */

      if (!gh_string_p (str))
	end_volta_span_p_->set_grob_property ("last-volta", SCM_BOOL_T);
    }

  if (gh_string_p (str))
    {
      started_mom_ = now;
      if (volta_span_p_)
	{
	  warning (_ ("Already have a volta spanner. Stopping that one prematurely."));

	  if (end_volta_span_p_)
	    {
	      warning (_("Also have a stopped spanner. Giving up."));

	      return ;

	    }

	  end_volta_span_p_ = volta_span_p_;
	  volta_span_p_ = 0;
	}

      volta_span_p_ = new Spanner (get_property ("VoltaBracket"));
      Volta_spanner::set_interface (volta_span_p_);
      announce_grob (volta_span_p_,0);
      volta_span_p_->set_grob_property ("text", str);
    }
}

void
Volta_engraver::acknowledge_grob (Grob_info i)
{
  if (Item* item = dynamic_cast<Item*> (i.elem_l_))
    {
      if (Note_column::has_interface (item))
	{
	  if (volta_span_p_)
	    Volta_spanner::add_column (volta_span_p_,item);
	  if (end_volta_span_p_)
	    Volta_spanner::add_column (end_volta_span_p_,item);      
	}
      if (Bar::has_interface (item))
	{
	  if (volta_span_p_)
	    Volta_spanner::add_bar (volta_span_p_, item);
	  if (end_volta_span_p_)
	    Volta_spanner::add_bar(end_volta_span_p_ , item);
	}
    }
}

void
Volta_engraver::finalize ()
{
  if (volta_span_p_)
    {
      typeset_grob(volta_span_p_);
    }
  if (end_volta_span_p_)
    {
      typeset_grob (end_volta_span_p_);
    }
}

void
Volta_engraver::start_translation_timestep ()
{
  first_b_ = true;
}

void 
Volta_engraver::stop_translation_timestep ()
{
  if (end_volta_span_p_)
    {
      Side_position::add_staff_support (end_volta_span_p_);
      
      typeset_grob (end_volta_span_p_ );
      end_volta_span_p_ =0;
    }
}

/*
  TODO: should attach volta to paper-column if no bar is found.
 */
