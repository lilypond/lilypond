/*   
unfolded-repeat-iterator.cc --  implement Unfolded_repeat_iterator, Volta_repeat_iterator

source file of the GNU LilyPond music typesetter


(c) 2002--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "music.hh"
#include "sequential-iterator.hh"
#include "context.hh"

class Unfolded_repeat_iterator : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  virtual SCM get_music_list () const;
};


SCM
Unfolded_repeat_iterator::get_music_list () const
{
  SCM l = SCM_EOL;
  SCM *tail = &l;
  
  SCM body = get_music ()->get_property ("element");
  SCM alts = get_music ()->get_property ("elements");
  int alt_count = scm_ilength (alts);
  int rep_count = ly_scm2int (get_music ()->get_property ("repeat-count"));

  for (int i = 0; i < rep_count; i++)
    {
      if (unsmob_music (body))
	*tail = scm_cons (body, SCM_EOL) ;

      tail = SCM_CDRLOC (*tail);

      if (alt_count)
	{
	  *tail = scm_cons (ly_car (alts), SCM_EOL);
	  tail = SCM_CDRLOC (*tail);
	  if (i >= rep_count - alt_count)
	    
	    alts = ly_cdr (alts);
	}      
    }

  return l; 
}

class Volta_repeat_iterator : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Volta_repeat_iterator ();

  void add_repeat_command (SCM);
protected:
  virtual SCM get_music_list () const;
  virtual void next_element (bool);
  virtual void construct_children ();
  virtual void process (Moment);

  bool first_time_;
  int alt_count_;
  int rep_count_;
  int done_count_;
};


Volta_repeat_iterator::Volta_repeat_iterator ()
{
  done_count_ = alt_count_ = rep_count_= 0;
  first_time_ = true;
}

SCM
Volta_repeat_iterator::get_music_list ()const
{
  return scm_cons (get_music ()->get_property ("element"),
		  get_music ()->get_property ("elements"));
}

void
Volta_repeat_iterator::construct_children ()
{
  Sequential_iterator::construct_children ();
  
  SCM alts = get_music ()->get_property ("elements");

  alt_count_ = scm_ilength (alts);
  rep_count_ = ly_scm2int (get_music ()->get_property ("repeat-count"));
  done_count_ = 0;
}


/*
  TODO: add source information for debugging
 */
void
Volta_repeat_iterator::add_repeat_command (SCM what)
{
  SCM reps = ly_symbol2scm ("repeatCommands");
  SCM current_reps = get_outlet ()->internal_get_property (reps);

  Context * where = get_outlet ()->where_defined (reps);
  if (where
      && current_reps == SCM_EOL || ly_c_pair_p (current_reps))
    {
      current_reps = scm_cons (what, current_reps);
      where->internal_set_property (reps, current_reps);
    }
}


void
Volta_repeat_iterator::next_element (bool side_effect)
{
  done_count_ ++;

  Sequential_iterator::next_element (side_effect);

  if (side_effect)
    {    
      if (alt_count_)
	{
	  String repstr = to_string (rep_count_ - alt_count_ + done_count_) + ".";
	  if (done_count_ > 1)
	    {
	      add_repeat_command (scm_list_n (ly_symbol2scm ("volta"), SCM_BOOL_F, SCM_UNDEFINED));

	      if (done_count_ - 1 < alt_count_)
		add_repeat_command (ly_symbol2scm ("end-repeat"));
	    }

	  
      
	  if (done_count_ == 1 && alt_count_ < rep_count_)
	    {
	      repstr = "1.--" + to_string (rep_count_ - alt_count_ + done_count_) + ".";
	    }

	  if (done_count_ <= alt_count_)
	    add_repeat_command (scm_list_n (ly_symbol2scm ("volta"),
					    scm_makfrom0str (repstr.to_str0 ()), SCM_UNDEFINED));
	}
      else
	{
	  add_repeat_command (ly_symbol2scm ("end-repeat"));
	}
    }
}


void
Volta_repeat_iterator::process (Moment m)
{
  if (first_time_)
    {
      add_repeat_command (ly_symbol2scm ("start-repeat"));
      first_time_ = false;
    }
  Sequential_iterator::process (m);
}


IMPLEMENT_CTOR_CALLBACK (Volta_repeat_iterator);
IMPLEMENT_CTOR_CALLBACK (Unfolded_repeat_iterator);
