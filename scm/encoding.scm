;;;; encoding.scm -- font encoding
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004--2008 Jan Nieuwenhuizen <janneke@gnu.org>



(define-public latin1-coding-vector #(.notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef space exclam quotedbl numbersign dollar percent ampersand quoteright parenleft parenright asterisk plus comma hyphen period slash zero one two three four five six seven eight nine colon semicolon less equal greater question at A B C D E F G H I J K L M N O P Q R S T U V W X Y Z bracketleft backslash bracketright asciicircum underscore quoteleft a b c d e f g h i j k l m n o p q r s t u v w x y z braceleft bar braceright asciitilde .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef dotlessi grave acute circumflex tilde macron breve dotaccent dieresis .notdef ring cedilla .notdef hungarumlaut ogonek caron space exclamdown cent sterling currency yen brokenbar section dieresis copyright ordfeminine guillemotleft logicalnot hyphen registered macron degree plusminus twosuperior threesuperior acute mu paragraph periodcentered cedilla onesuperior ordmasculine guillemotright onequarter onehalf threequarters questiondown Agrave Aacute Acircumflex Atilde Adieresis Aring AE Ccedilla Egrave Eacute Ecircumflex Edieresis Igrave Iacute Icircumflex Idieresis Eth Ntilde Ograve Oacute Ocircumflex Otilde Odieresis multiply Oslash Ugrave Uacute Ucircumflex Udieresis Yacute Thorn germandbls agrave aacute acircumflex atilde adieresis aring ae ccedilla egrave eacute ecircumflex edieresis igrave iacute icircumflex idieresis eth ntilde ograve oacute ocircumflex otilde odieresis divide oslash ugrave uacute ucircumflex udieresis yacute thorn ydieresis))


(define-public (decode-byte-string str)
  "Return vector of glyphname symbols that correspond to string,
assuming that STR is byte-coded using ENCODING-NAME."

  (let* ((len (string-length str))
	 (output-vector (make-vector len '.notdef)))

    (do
	((idx 0 (1+ idx)))
	((>= idx len) output-vector)
      (vector-set! output-vector idx
		     (vector-ref latin1-coding-vector
				 (char->integer (string-ref str idx)))))))
