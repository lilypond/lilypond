/*   
unfolded-repeat-iterator.cc --  implement Unfolded_repeat_iterator, Volta_repeat_iterator

source file of the GNU LilyPond music typesetter


(c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "music.hh"
#include "sequential-iterator.hh"
#include "translator-group.hh"

class Unfolded_repeat_iterator : public Sequential_iterator
{
public:
  static  SCM constructor_cxx_function;
  VIRTUAL_COPY_CONS (Music_iterator);
protected:
  virtual SCM get_music_list () const;
};


SCM
Unfolded_repeat_iterator::get_music_list () const
{
  SCM l = SCM_EOL;
  SCM *tail = &l;
  
  SCM body = music_l ()->get_mus_property ("element");
  SCM alts = music_l ()->get_mus_property ("elements");
  int alt_count = scm_ilength (alts);
  int rep_count = gh_scm2int (music_l ()->get_mus_property ("repeat-count"));

  for (int i = 0; i < rep_count; i++)
    {
      if (unsmob_music (body))
	*tail = gh_cons (body, SCM_EOL) ;

      tail = SCM_CDRLOC (*tail);

      if (alt_count)
	{
	  *tail = gh_cons (gh_car (alts), SCM_EOL);
	  tail = SCM_CDRLOC (*tail);
	  if (i >= rep_count - alt_count)
	    
	    alts = gh_cdr (alts);
	}      
    }

  return l; 
}

class Volta_repeat_iterator : public Sequential_iterator
{
public:
  static  SCM constructor_cxx_function;
  VIRTUAL_COPY_CONS (Music_iterator);
  Volta_repeat_iterator();

  void add_repeat_command (SCM);
protected:
  virtual SCM get_music_list () const;
  virtual void next_element (bool);
  virtual void construct_children();
  virtual void process (Moment);
  
  int alt_count_;
  int rep_count_;
  int done_count_;
};


Volta_repeat_iterator::Volta_repeat_iterator()
{
  done_count_ = alt_count_ = rep_count_= 0;
}

SCM
Volta_repeat_iterator::get_music_list()const
{
  return gh_cons (music_l ()->get_mus_property ("element"),
		  music_l ()->get_mus_property ("elements"));
}

void
Volta_repeat_iterator::construct_children ()
{
  Sequential_iterator::construct_children();
  
  SCM alts = music_l ()->get_mus_property ("elements");

  alt_count_ = scm_ilength (alts);
  rep_count_ = gh_scm2int (music_l ()->get_mus_property ("repeat-count"));
  done_count_ = 0;
}


/*
  TODO: add source information for debugging
 */
void
Volta_repeat_iterator::add_repeat_command (SCM what)
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
Volta_repeat_iterator::next_element (bool side_effect)
{
  done_count_ ++;

  Sequential_iterator::next_element (side_effect);

  if (side_effect)
    {    
      if (alt_count_)
	{
	  String repstr = to_str (rep_count_ - alt_count_ + done_count_) + ".";
	  if (done_count_ > 1)
	    {
	      add_repeat_command (scm_list_n (ly_symbol2scm ("volta"), SCM_BOOL_F, SCM_UNDEFINED));

	      if (done_count_ - 1 < alt_count_)
		add_repeat_command (ly_symbol2scm ("end-repeat"));
	    }

	  
      
	  if (done_count_ == 1 && alt_count_ < rep_count_)
	    {
	      repstr = "1.--" + to_str (rep_count_ - alt_count_ + done_count_) + ".";
	    }

	  if (done_count_ <= alt_count_)
	    add_repeat_command (scm_list_n (ly_symbol2scm ("volta"),
					    ly_str02scm (repstr.ch_C ()), SCM_UNDEFINED));
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
  if (!m.to_bool ())
    {
      add_repeat_command (ly_symbol2scm ("start-repeat"));
    }
  Sequential_iterator::process(m);
}


IMPLEMENT_CTOR_CALLBACK(Volta_repeat_iterator);
IMPLEMENT_CTOR_CALLBACK(Unfolded_repeat_iterator);
