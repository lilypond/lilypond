/*   
  auto-change-iterator.hh -- declare Auto_change_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AUTO_CHANGE_ITERATOR_HH
#define AUTO_CHANGE_ITERATOR_HH

#include "music-wrapper-iterator.hh"
#include "direction.hh"

class Auto_change_iterator : public Music_wrapper_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  Auto_change_iterator ();

protected:
  virtual void process (Moment);  

private:
  Direction where_dir_;
  void change_to (Music_iterator* , String, String);
};

#endif /* AUTO_CHANGE_ITERATOR_HH */
