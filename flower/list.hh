// list.hh

#ifndef __LIST_HH
#define __LIST_HH

class ostream;
template<class T> class Cursor;
template<class T> class Link;

/// all purpose list
template<class T>
class List
{
 public:
     /// construct empty list                
    List(); 

    /// construct list from first item.  
    List( const T& thing );
    
    virtual ~List();
	
    Cursor<T> bottom();

    int size() const;
    Cursor<T> top();
    void OK() const; 
 protected:
    friend class Cursor<T>;
    friend class Link<T>;
    
    /// add after after_me
    void add( const T& thing, Cursor<T> after_me );

    /// put thing before #before_me#
    void insert( const T& thing, Cursor<T> before_me );
    virtual void remove( Cursor<T> me );
    /**
      Remove link pointed to by me.

      POST
      none; WARNING: do not use #me#.
     */
    int size_;
    Link<T>* top_;
    Link<T>* bottom_;
};
/**
  a doubly linked list; 
  List can be seen as all items written down on paper,
  from top to bottom

  class Cursor is used to extend List

   items are always stored as copies in List, but:
   #List<String># :  copies of #String# stored 
   #List<String*># : copies of #String*# stored! 
   (do not use, use \Ref{PointerList}#<String*># instead.)
 
   {\bf note:} 
   retrieving "invalid" cursors, i.e. 
   #top()/bottom()# from empty list, #find()# without success,
    results in a nonvalid Cursor ( #!ok()# )


    INVARIANTEN!
*/


/// Use for list of pointers, e.g. PointerList<AbstractType*>.
template<class T>
class PointerList : public List<T>
{
 public:
    PointerList();
    PointerList( const T& thing );

    ///
    virtual ~PointerList();
    /**
      This function deletes deletes the allocated pointers of all links. 
      #\Ref{~List}# is used to delete the links themselves.
      */ 

 protected:
    virtual void remove( Cursor<T> me );
};

#include "list.inl"
#include "cursor.hh"

// instantiate a template:  explicit instantiation.
#define L_instantiate(a)  template class List<a>; template class Cursor<a>; \
  template class Link<a>
#define PL_instantiate(a) L_instantiate(a *); template class PointerList<a*>

#endif // __LIST_HH //
    
   


