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
    Cursor( List<T>& list, Link<T>* pointer = 0 );
    Cursor( const Cursor<T>& cursor );
    
    /// return current T
    T& operator *();		
    operator T() { return  *(*this); }
    Cursor<T> operator =( const Cursor<T>& c );

    /// make cursor with #no# items back
    Cursor<T> operator -( int no) const;

    /// make cursor with #no# items further
    Cursor<T> operator +( int no) const;

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
    /// remove and cleanup Link // HWN: backspace or del?
    void remove();		

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



/// cursor which feels like a pointer
template<class T>
struct PCursor : public Cursor<T> {

    /// make cursor with #no# items back
    PCursor<T> operator -( int no) const {
	return PCursor<T> (Cursor<T>::operator-(no));
    }

    /// make cursor with #no# items further
    PCursor<T> operator +( int no) const {
	return PCursor<T> (Cursor<T>::operator+(no));
    }
    PCursor(List<T> & l) : Cursor<T> (l) {}

    PCursor( const Cursor<T>& cursor ) : Cursor<T>(cursor) { }
    T operator ->() { return  *(*this); }

};
/**
 HWN: I'd like an operator->(), so here it is.

 Cursor to go with pointer list.
 */

#include "list.hh"
#include "cursor.inl"

#endif // __CURSOR_HH //
