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
  virtual bool do_try_music (Music*);
};

ADD_THIS_TRANSLATOR (A2_devnull_engraver);

bool
A2_devnull_engraver::do_try_music (Music *m) 
{
  if (Note_req * n = dynamic_cast <Note_req *> (m))
    {
      SCM a2 = get_property ("a2");
      // should be able to read id_str_, no?
      SCM second = get_property ("second");

      if (a2 == SCM_BOOL_T && second == SCM_BOOL_T)
	{
	  return true;
	}
    }
  return false;
}
      
