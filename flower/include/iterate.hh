/*
  iterate.hh -- define some list macros

  source file of the flowerlib

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef ITERATE_HH
#define ITERATE_HH

/*
 ugh.
 jcn: kjoet.
      if we wouldn't have had this, things might have been 
      just a little bit easier to read, imho.
      (it does save quite some disk space, though)
 */

#define iterator(set)		typeof ((set).top())
#define iterator_bot(set)		typeof ((set).bottom())

#define iter(init, var)		typeof (init) var (init)

// should use top()
#define iter_top(set,var)	iterator (set) var (set)
#define iter_bot(set,var)	iterator (set) var (set.bottom())

#endif // ITERATE_HH
