/*
  tuple.hh -- declare Tuple

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Han-Wen Nienhuys <hanwen@lilypond.org>

*/

#ifndef TUPLE_HH
#define TUPLE_HH

template<class T, int N>
struct Tuple
{
  T t_array[N];
  Tuple ()
  {
  }
  
  Tuple (T const *src)
  {
    for (int i = 0; i < N; i++)
      t_array[i] = src[i];
  }
};

template<class K>
struct Tuple2 : public Tuple<K, 2>
{
  Tuple2 ()
  {

  }
  
  Tuple2 (K a, K b)
  {
    Tuple<K,2> *p(this);	//  ugr.
    
    p->t_array[0] = a;
    p->t_array[1] = b;
  }
};

  
template<class T, int N>
inline bool
operator<(Tuple<T, N> const &t1,
	  Tuple<T, N> const &t2)
{
  for (int i = 0; i < N ; i++)
    {
      if (t1.t_array[i] > t2.t_array[i])
	return false;
      if (t1.t_array[i] < t2.t_array[i])
	return true;
    }

  return false;
}

#endif /* TUPLE_HH */
