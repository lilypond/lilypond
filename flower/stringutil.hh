#ifndef STRINGUTIL_HH
#define STRINGUTIL_HH
#include <assert.h>

#if !defined(NDEBUG)
#define NDEBUG BLONDE
#endif

const INITIALMAX=8;
class String_handle;
/// Internal String struct
class StringData {
    // GNU malloc: storage overhead is 8 bytes anyway.


friend class String_handle;
    int maxlen;	// maxlen is arraysize-1
    
    int length;
    char* string;
    int references;

    /// init to ""
    StringData() {
	references=0;
	maxlen = INITIALMAX;
	string = new char[maxlen + 1];
	string[0] = 0;
	length = 0;
    }

    /// init from src. Conservative allocation.
    StringData(StringData const &src) {
	references=0;	
	maxlen = length = src.length;		
	string = new char[maxlen+1]; // should calc GNU 8byte overhead. 	
	strcpy(string, src.string);	
    }
    
    ~StringData() {
	assert(references == 0);
	delete[] string;
    }


    void setmax(int j) {	
	OKW();
	if (j > maxlen) {
	    delete string;
	    maxlen = j;
	    string = new char[maxlen + 1];
	
	    string[0] = 0;
	    length = 0;
	}
    }
    /** POST: maxlen >= j.
      IN: j, maximum stringlength.    
      contents thrown away.
    */
    ///
    void remax(int j) {
	OKW();
	if (j > maxlen) {
	    maxlen = j;
	    char *p = new char[maxlen + 1];	    
	    strcpy(p,string);	    
	    delete[] string;
	    string = p;
	//    length = strlen(string);
	}
    }
    /** POST: maxlen >= j.
      IN: j, maximum stringlength.
      contents are kept if it grows.
      */    
    /// check if writeable.
    void OKW() {

	assert (references == 1);

    }

    /// check state.
    void OK() {
	assert(strlen(string) == size_t(length));
	assert(maxlen >= length);
	assert(bool(string));
	assert(references >= 1);
    }

    // needed?
    void update() {
	length  = strlen (string);
    }

    /// reduce memory usage.
    void tighten() { // should be dec'd const
	maxlen = length;
	char *p = new char[maxlen + 1];	    
	strcpy(p,string);	    
	delete[] string;
	string = p;		
    }

    // assignment.
    void set(const char *s) {
	OKW();

	assert(s);

	length = strlen (s);
	remax(length);
	strcpy(string,s);
    }
    
    /// concatenation.
    void operator += (const char *s) {
	OK();
	OKW();
	int old = length;
	
	length += strlen(s);
	remax (length);
	strcpy(string + old, s);	
    }

    /// the array itself
    operator const char *() const { return string; }

    // idem, non const
    char *array_for_modify() {
	OKW();
	return string;
    }
    void trunc(int j) {
	OKW(); 
	assert(j >= 0 && j <= length);
	string[j] = 0;
	length = j;
    }

    /** not really safe. Can alter length without StringData knowing it.
      */
    char &operator [](int j) {
	assert(j >= 0 && j <= length);
	return string[j] ; 
    }

    char operator [](int j) const {
	assert(j >= 0 && j <= length);
	return string[j]; 
    }
};

/**
   the data itself. Handles simple tasks (resizing, resetting)
   */


/****************************************************************/
/// ref. counting for strings
class String_handle {
    StringData* data;
    
    /// decrease ref count. Named kind of like a Tanenbaum semafore 
    void down() { if (!(--data->references)) delete data; data = 0; }

    /// increase ref count
    void up(StringData *d) { data=d; data->references ++; }
    
    /** make sure data has only one reference.      
       POST: data->references == 1
      */
    void copy() {
	if (data->references !=1){
	    StringData *newdata = new StringData(*data);
	    down();
	    up(newdata);
	}
    }
    
public:

    String_handle() {
	up(new StringData);
    }
    ~String_handle() {	
	down();
    }    
    String_handle(String_handle const & src) {	
	up(src.data);
    }

    /// retrieve the actual array.
    operator const char *() const { return *data; }    
    char *array_for_modify() {
	copy();
	return data->array_for_modify();
    }
    
    void operator =(String_handle const &src) {
	if (this == &src)
	    return;
	down();
	up(src.data);
    }
    
    void operator += (const char *s) {	
	copy();
	*data += s;
    }    

    
    char operator[](int j) const { return (*data)[j]; }

    // !NOT SAFE!
    // don't use this for loops. Use array_for_modify()
    char &operator[](int j) {
	copy(); 	// hmm. Not efficient
	return data->array_for_modify()[j];
    }

    void operator = (char const *p) {
	copy();
	data->set(p);
    }
			       
    void trunc(int j) { copy(); data->trunc(j); }
    int len() const { return data->length; }
};
/**
   handles ref. counting, and provides a very thin
   interface using char *
 */


#ifdef NDEBUG
#if (NDEBUG == BLONDE)
#undef NDEBUG
#endif
#endif



#endif // STRINGUTIL_HH
