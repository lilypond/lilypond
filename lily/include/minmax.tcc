
/*   
  minmax.cc --  implement minmax()
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
  
 */

template<class T>
minmax (Direction d, T t1, T t2)
{
  if (d > 0) return t1 >? t2;
  else return t1 <? t2;
}

