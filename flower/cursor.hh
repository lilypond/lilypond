// cursor.hh

#ifndef __CURSOR_HH
#define __CURSOR_HH

#include "link.hh"
template<class T> class List;

///
template<class T>
class Cursor 
{
 public:
    Cursor( const List<T>& list, Link<T>* pointer = 0 );
    /** this isn't true, actually, #list# surely isn't const, but I get
      tired of the warning messages.  */
    
    Cursor( const Cursor<T>& cursor );

    T& thing();
    /// return current T
    T& operator *() { return thing(); }
    operator T() { return thing(); }
    Cursor<T> operator =( const Cursor<T>& c );

    /// make cursor with #no# items back
    Cursor<T> operator -( int no) const;

    /// make cursor with #no# items further
    Cursor<T> operator +( int no) const;
    int operator -(Cursor<T> op) const;
    Cursor<T> operator -=(int);
    Cursor<T> operator +=(int);
    
    /// return current and move one down
    Cursor<T> operator ++( int );
    
    /// return current and move one up
    Cursor<T> operator --( int ); 

    /// point to link?
    bool ok();                  

    /// ++ items left?
    bool forward();		

    /// -- items left?
    bool backward();

    /// put (copy) after me in List
    void add( const T& thing );
    /**
      analogously to editor. ok() interpreted as at end
      of line.

      PRE: !ok, POST: added to bottom()

      PRE: ok, POST: added after me

      cursor points to same object, cursor.next() is newly added
      object.
      */

    /// put (copy) before me in List
    void insert( const T& thing );
    /**
      analogously to editor. ok() interpreted as at begin of
      line.
      
      PRE: !ok, POST: add to top()

      PRE: ok, POST: add before me

      cursor points to same object, cursor.previous()
      is newly inserted object.
      */
    
    ///
    void backspace();

    /// 
    void del();
    
    /// access the list this came from
    const List<T>& list() const ;
    Link<T>* pointer();

private:
    List<T>& list_;
    Link<T>* pointer_;
};
/** 
  add and insert extend the list
  items are always stored as copies in List, but:
  List<String> :  copies of String stored 
  List<String*> : copies of String* stored!

    the operations add and insert actually delegate the work to List class.
 */


/*
  comparisons.
  */
#include "compare.hh"

template<class T>
inline  int cursor_compare(Cursor<T> a,Cursor<T>b)
{
    return a-b;
}

template_instantiate_compare(Cursor<T>, cursor_compare, template<class T>);

#include "pcursor.hh"
#include "list.hh"
#include "cursor.inl"

#endif // CURSOR_HH 
