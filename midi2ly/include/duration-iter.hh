/*   
  duration-iter.hh -- declare Duration_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#ifndef DURATION_ITER_HH
#define DURATION_ITER_HH

/// (iter_dur)
struct Duration_iterator {
  
  /// start at shortest: 128:2/3
  Duration_iterator ();

  // **** what about these three here ?
  /// return forward_dur ();
  Duration operator ++(int); 

  /// return ok ()
  operator bool (); 

  /// return dur ()
  Duration operator ()(); 


  /// return current dur
  Duration dur ();

  /// return dur (), step to next
  Duration forward_dur ();

  /// durations left?
  bool ok ();

private:

  Duration cursor_dur_;
};



#endif /* DURATION_ITER_HH */

