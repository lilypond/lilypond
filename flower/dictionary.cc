
#if 0


/**
   Dictionary implementation.  Please fix me.

   (neuk. hsearch_* is te dom.)
 */
template<class T>
class Dictionary<T>
{
  hsearch_data * hash_p_;
  
public:
  Dictionary ();
  ~Dictionary ();
  Dictionary (Dictionary const&);
  T &elem (String s);
  T const &elem (String s) const;
  bool elem_b (String s) const;
  void add (String, T);
  void clear ();
}

Dictionary::Dictionary ()
{
  hash_p_ = new hsearch_data;
  hash_p_->table = 0;

  int start_size = 51;
  int retval = hcreate_r (start_size, hash_p_);

  assert (retval);
}



#endif
