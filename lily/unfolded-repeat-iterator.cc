/*   
  unfolded-repeat-iterator.cc --  implement Unfolded_repeat_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

/*
  This is too hairy.  Maybe split into subclasses for volta and full
  unfold?
  
 */
#include "music-iterator.hh"
#include "repeated-music.hh"
#include "music-list.hh"
#include "debug.hh"
#include "translator-group.hh"

/**
   Iterate repeats.  First do body, then alternatives one by one,
   optionally interspersed by the body.
 */
class Unfolded_repeat_iterator : public Music_iterator
{
  void add_repeat_command (SCM);

public:
  VIRTUAL_COPY_CONS (Music_iterator);
  /**
     How often have we done the body (assuming bodies are interspersed.)?

     In volta: the number to print in the bracket.
   */
  int done_count_;
  static SCM constructor_cxx_function; 

  /*
    are we now busy doing the body?

   */
  bool do_main_b_;

  /*
    are we doing volta's?
   */
  bool volta_b_;

  /** How far have we progressed into the repeat.
      This excludes the elt currently being iterated.
  */
  Moment here_mom_;
  int alternative_count_i_;
  Music_iterator * current_iter_p_;
  
  /// pointer to the alternative that will be processed next.
  SCM alternative_cons_;
  ~Unfolded_repeat_iterator ();
  Unfolded_repeat_iterator ();
  Unfolded_repeat_iterator (Unfolded_repeat_iterator const &);
protected:  
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void process (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;
  virtual void skip (Moment);
  virtual SCM get_music (Moment) const;
  
  virtual bool ok () const;
  virtual void next_element (bool side_effect);
};

class Volta_repeat_iterator : public Unfolded_repeat_iterator
{
public:
  Volta_repeat_iterator ();
  static  SCM constructor_cxx_function;
  VIRTUAL_COPY_CONS (Music_iterator);
};



Unfolded_repeat_iterator::~Unfolded_repeat_iterator ()
{
  delete current_iter_p_;
}

Unfolded_repeat_iterator::Unfolded_repeat_iterator (Unfolded_repeat_iterator const &src)
  : Music_iterator (src)
{
  done_count_ = src.done_count_;
  current_iter_p_ = (src.current_iter_p_)? src.current_iter_p_->clone () : 0;
  do_main_b_ = src.do_main_b_;
  volta_b_ = src.volta_b_;
  alternative_count_i_ = src.alternative_count_i_;
  alternative_cons_ = src.alternative_cons_;
}

Unfolded_repeat_iterator::Unfolded_repeat_iterator ()
{
  done_count_ =0;
  current_iter_p_ =0;
  volta_b_ = false;
  do_main_b_ = false;
  alternative_count_i_ =0;
  alternative_cons_ = SCM_EOL;
}

/**

If we are in the body of the repeat always go to the current alternative.

If we are not in the body, then we are in an alternative.  If we are
fully unfolding, advance the current alternative and go back to main.
If we are semi-unfolding, advance the current alternative, and go to
the  alternative just set.
   
 */
void
Unfolded_repeat_iterator::next_element (bool side_effect) 
{
  Repeated_music * repmus =dynamic_cast<Repeated_music *> (music_l ());
  delete current_iter_p_;
  current_iter_p_ =0;

  bool do_repcommands = side_effect && volta_b_;
  
  if (do_main_b_)
    {
      /*
	we were busy doing the main body, so

	- go to alternative if we're a volta

	- make a :| if there are no alternatives   
	
	- do something intelligent when we're fully unfolding (fixcomment)
       */
      
      here_mom_ += repmus->body ()->length_mom ();

      if (!volta_b_)
	done_count_ ++;
     
      if (gh_pair_p (alternative_cons_))
	{
	  current_iter_p_ = get_iterator_p (unsmob_music (ly_car (alternative_cons_)));
	  do_main_b_ = false;

	  if (volta_b_)
	    {
	      String repstr = to_str (done_count_ + 1) + ".";

	      /*
		we're coming in from main, so we're always on the first repeat.
	       */
	      assert (done_count_ == 0);

	      if (done_count_ == 0
		  && alternative_count_i_ < repmus->repeat_count ())
		{
		  done_count_ += repmus->repeat_count () - alternative_count_i_;		  
		  repstr = "1.--" + to_str (done_count_ + 1) + ".";		  
		}		  
	      
	      if (do_repcommands)
		add_repeat_command (scm_list_n (ly_symbol2scm ("volta"),
					     ly_str02scm (repstr.ch_C ()), SCM_UNDEFINED));
	    }	  
	}
      else if (volta_b_)
	{
	  add_repeat_command (ly_symbol2scm ("end-repeat"));
	}
      else if (done_count_ <  repmus->repeat_count ())
	{
	  current_iter_p_ = get_iterator_p (repmus->body ());
	  do_main_b_ = true;
	}
    }
  else
    {
      /*
	we're not in the main part. So we're either in an alternative, or
	we just finished.
      */

      /*
	we're in the alternatives.  We move the pointer to the
	next alternative.
       */
      if (alternative_cons_)
	{
	  here_mom_ += unsmob_music (ly_car (alternative_cons_))->length_mom ();

	  if (volta_b_ || 
	      repmus->repeat_count () - done_count_  < alternative_count_i_)
	    alternative_cons_ = ly_cdr (alternative_cons_);
	  
	  if (do_repcommands)
	    add_repeat_command (scm_list_n (ly_symbol2scm ("volta"), SCM_BOOL_F, SCM_UNDEFINED));

	  
	  
	  /*
	    we've done the main body as well, but didn't go over the other
	    increment.  */
	  if (volta_b_)
	    done_count_ ++;
	}

      /*
	We still have alternatives left, so

	if we're volta: traverse them

	if we're full unfold: go back to main body.
       */
      
      if (done_count_ < repmus->repeat_count () && gh_pair_p (alternative_cons_))
	{
	  if (do_repcommands)
	    {
	      String repstr = to_str (done_count_ + 1) + ".";
	      add_repeat_command (scm_list_n (ly_symbol2scm ("volta"),
					   ly_str02scm (repstr.ch_C ()), SCM_UNDEFINED));
	      add_repeat_command (ly_symbol2scm ("end-repeat"));
	    }

	  
	  if (volta_b_)
	    current_iter_p_ = get_iterator_p (unsmob_music (ly_car (alternative_cons_)));
	  else
	    {
	      current_iter_p_ = get_iterator_p (repmus->body ());
	      do_main_b_ = true;
	    }
	}
    }
}


bool
Unfolded_repeat_iterator::ok () const
{
  return current_iter_p_;
}

Moment
Unfolded_repeat_iterator::pending_moment () const
{
  return here_mom_ + current_iter_p_->pending_moment ();
}

void
Unfolded_repeat_iterator::construct_children ()
{
  Repeated_music * mus =dynamic_cast<Repeated_music *> (music_l ());
  
  alternative_cons_ = (mus->alternatives ())
    ? mus->alternatives ()
    : SCM_EOL;

  for (SCM p = alternative_cons_; gh_pair_p (p); p = ly_cdr (p))
    alternative_count_i_ ++;

  if (mus->body ())
    {
      current_iter_p_  = get_iterator_p (mus->body ());
      do_main_b_ = true;
    }
  else if (gh_pair_p (alternative_cons_))
    {
      current_iter_p_ = get_iterator_p (unsmob_music (ly_car (alternative_cons_)));
      do_main_b_ = false;
    }

  while (current_iter_p_ && !current_iter_p_-> ok ())
    {
      next_element (true);
    }
}

/*
  TODO: add source information for debugging
 */
void
Unfolded_repeat_iterator::add_repeat_command (SCM what)
{
  SCM reps = ly_symbol2scm ("repeatCommands");
  SCM current_reps = report_to_l ()->internal_get_property (reps);

  Translator_group * where = report_to_l ()->where_defined (reps);
  if (where
      && current_reps == SCM_EOL || gh_pair_p (current_reps))
    {
      current_reps = gh_cons (what, current_reps);
      where->internal_set_property (reps, current_reps);
    }
}

void
Unfolded_repeat_iterator::process (Moment m) 
{
  if (!m.to_bool ())
    {
      if (volta_b_)
	add_repeat_command (ly_symbol2scm ("start-repeat"));
    }
  while (1)
    {
      while (!current_iter_p_->ok ())
	{
	  next_element (true);

	  if (!current_iter_p_)
	    return;
	}
      
      if (m - here_mom_ >= current_iter_p_->pending_moment ())
	current_iter_p_->process (m - here_mom_);
      else
	return;
    }
}

void
Unfolded_repeat_iterator::skip (Moment until)
{
  while (current_iter_p_)
    {
      Moment l =current_iter_p_->music_length_mom ();
      if (l >= until - here_mom_)
	current_iter_p_->skip (until - here_mom_);

      if (current_iter_p_->ok ())
	return ; 

      next_element (false);
    }
}

SCM
Unfolded_repeat_iterator::get_music (Moment until)const
{
  SCM s = SCM_EOL;
  if (until <  pending_moment ())
    return s;


  Unfolded_repeat_iterator * me
    = dynamic_cast<Unfolded_repeat_iterator*> (this->clone ());
  
  while (me->ok ())
    {
      SCM nm = me->current_iter_p_->get_music (until -
					       me->here_mom_);
      
      s = gh_append2 (nm, s);
      
      Moment m = 0;
      for (SCM i = nm; gh_pair_p (i); i = ly_cdr (i))
	m = m >? unsmob_music (ly_car (i))->length_mom ();

      if (m > Moment (0))
	break ;
      else
	me->next_element (false);
    }

  delete me;
  
  return s;
}


Music_iterator* 
Unfolded_repeat_iterator::try_music_in_children (Music  * m) const
{
  return  current_iter_p_->try_music (m);
}

IMPLEMENT_CTOR_CALLBACK (Unfolded_repeat_iterator);
IMPLEMENT_CTOR_CALLBACK (Volta_repeat_iterator);

Volta_repeat_iterator::Volta_repeat_iterator ()
{
  volta_b_ = true;
}
