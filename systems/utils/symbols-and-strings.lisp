;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================
(in-package :utils)

;; ############################################################################

;; ----------------------------------------------------------------------------
;; configuration:

(defvar *default-id-base-name* "ID")
(defvar *default-var-id-base-name* "?VAR")
(defvar *default-const-id-base-name* "CONST")
(defvar *default-sticker-name* "STICKER")
;; ----------------------------------------------------------------------------
;; private system functionality:

(defparameter *nid-table* (make-hash-table :test #'equal))

(proclaim '(inline get-next-id-number))
(defun get-next-id-number (name)
  "Return the next number to create the next unique id with the given name."
  (declare (type string name))
  (if (gethash name *nid-table*)
      (incf (gethash name *nid-table*))
      (setf (gethash name *nid-table*) 1)))

(proclaim '(inline remove-numeric-tail))
(defun remove-numeric-tail (name)
  (declare (type string name))
  (loop for i from (- (length name) 1) downto 0
        for char = (elt name i)
        when (not (digit-char-p char))
        do (if (equal #\- char)
             (return (subseq name 0 i))
             (return name))
        finally (return name)))

(proclaim '(inline get-base-name))
(defun get-base-name (name &key
                      (remove-numeric-tail t)
                      (remove-question-mark t))
  "Return the base of the given name.
   - If base is a symbol then the base name is the symbol's name.
   - If base is a string then this string is the base name.
   - If remove-question-mark is true and the base name starts with a
     question-mark then this question-mark is removed from the base name.
   - If remove-numeric-tail is true and name is of the form 's-n',
     where s is a string of alphanumerical characters, and n is a string of
     numerical character, then the base is 's', i.e. the hyphen and trailing
     numerical characters are removed."
  (declare (type (or string symbol) name))
  (let* ((name (cond ((stringp name) (upcase name))
                    ((symbolp name) (symbol-name name))
                    (t (write-to-string name))))
        (name-as-string name))
    (if remove-numeric-tail (setq name (remove-numeric-tail name)))
    (if (string= name "") ;; for symbols like -5
      name-as-string
      (if (and remove-question-mark (char-equal #\? (elt name 0)))
        (subseq name 1)
        name))))

;; ----------------------------------------------------------------------------
;; public utilities:

(export '(get-base-name
          make-id
          make-var
          make-kw
          variable-p
          make-const
          reset-id-counters
          string-append
          mkstr
          symb
          internal-symb
          derive-base-name-from-type))

(unless (fboundp 'string-append)
  (defun string-append (&rest strings)
    "concatenates strings"
    (format nil "~{~a~}" strings)))

(defun mkstr (&rest arguments)
  "Returns a string containing all arguments."
  (format nil "~{~a~}" arguments))

(defun symb (&rest arguments)
  "Make a fresh, uninterned symbol with symbol-name (mkstr arguments)."
  (make-symbol (format nil "~{~a~}" arguments)))

(defun internal-symb (&rest arguments)
  "Make and intern the symbol with symbol-name (mkstr arguments)."
  (intern (format nil "~{~a~}" arguments)))

(defun make-id (&optional name)
  "Create and return a unique numbered id, which is a symbol whose symbol-name
   consists of a name and a number."
  #-lispworks
  (declare (type (or symbol string null) name))
  (let ((base-name (cond ((null name) *default-id-base-name*)
                         ((symbolp name) (symbol-name name))
                         ((stringp name) name))))
    (make-symbol (format nil "~:@(~a~)-~a" base-name (get-next-id-number base-name)))))

(defun make-var (&optional name)
  "Create and return a unique FCG variable symbol.
   Note that if you have the choice between passing a string or a symbol as the
   argument to make-var, make-const or make-id, then pass it the string. If you
   pass it the symbol then the implementation will simply take the symbol-name
   from it further ignore the symbol."
  #-lispworks
  (declare (type (or null string symbol) name))
  (make-id (if name
               (format nil "?~a" (get-base-name name))
               *default-var-id-base-name*)))

(defun make-kw (name)
  "takes a string, e.g. \"test\" and turns it into a
   symbol interned in the keyword package, e.g. :test"
  (values (intern (string-upcase name)
                  :keyword)))

(defun variable-p (x)
  "Test whether x is a variable, i.e. whether it is a symbol of which the name
   starts with a question mark."
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun make-const (&optional name (remove-numeric-tail t))
  "Create and return a unique FCG constant symbol."
  #-lispworks
  (declare (type (or null string symbol) name))
  (make-id (if name
               (get-base-name name :remove-numeric-tail remove-numeric-tail)
               *default-const-id-base-name*)))

(defun reset-id-counters ()
  "Reset all the counters for the numbered ids."
  (setf *nid-table* (make-hash-table :test #'equal))
  t)

(defun derive-base-name-from-type (type)
  "Given a type in some form, return a string base name."
  (cond ((null type) "x")
        ((symbolp type) (symbol-name type))
        ((listp type) (derive-base-name-from-type (car type)))
        (t (error "Unexpected type, add case to derive-base-name-from-type"))))

;; (progn
;;   (reset-id-counters)
;;   (pprint
;;     (loop for variable in
;;           (list (make-symbol "?foo") (make-symbol "?foo") (make-symbol "?foo1")
;; 		   (make-symbol "?foo-bar") (make-symbol "?foo-bar1")
;; 		   (make-id '?baz) (make-id '?baz)
;; 		   (make-id '?foo-42) (make-id '?foo-42))
;;      collect (cons variable (make-var variable)))))
;;
;; result:
;;
;; ((#:?FOO . #:?FOO-1) (#:?FOO . #:?FOO-2) (#:?FOO1 . #:?FOO1-1)
;;  (#:?FOO-BAR . #:?FOO-BAR-1) (#:?FOO-BAR1 . #:?FOO-BAR1-1)
;;  (#:?BAZ-1 . #:?BAZ-3) (#:?BAZ-2 . #:?BAZ-4) (#:?FOO-42-1 . #:?FOO-42-3)
;;  (#:?FOO-42-2 . #:?FOO-42-4))


;; ############################################################################
;; string utilities:
;; ----------------------------------------------------------------------------

(export '(downcase
          upcase
          string-replace
          replace-char
          read-file-as-string
	  read-until
          stringify
          punctuation-p
          remove-punctuation
          lisp->camel-case
          camel-case->lisp
          remove-newlines
          remove-spurious-spaces))

(defun punctuation-p (char) (find char "*_.,;:`!?#-()\\\""))

(defun remove-spurious-spaces (sentence-as-string)
  (let ((list-representation (split-sequence:split-sequence #\Space sentence-as-string :remove-empty-subseqs t)))
    (format nil "~{~a~^ ~}" list-representation)))

(defun remove-punctuation (string)
  "Replace punctuation with spaces in string."
  (substitute-if #\space #'punctuation-p string))

(defun remove-newlines (string)
  "removes newlines inside string"
  (cl-ppcre:regex-replace-all "\\n" string ""))

(defun downcase (str)
  (format nil "~(~a~)" str))

(defun upcase (str)
  (format nil "~:@(~a~)" str))

(defun stringify (symbol-or-string)
  "Make a string from a symbol, number of string, similar to listify"
  (cond ((stringp symbol-or-string)
         symbol-or-string)
        ((numberp symbol-or-string)
         (mkstr symbol-or-string))
        (t
         (symbol-name symbol-or-string))))

(defun string-replace (str1 sub1 sub2)
  "Nondestructively replaces all occurences of sub1 in str1 by sub2"
  (let ((str1 (string str1))
        (str2 "")
        (sub1 (string sub1))
        (sub2 (string sub2))
        (index1 0))
    (loop
       if (string-equal str1 sub1
                        :start1 index1
                        :end1 (min (length str1)
                                   (+ index1 (length sub1))))
       do (setq str2 (concatenate 'string str2 sub2))
         (incf index1 (length sub1))
       else do 
         (setq str2 (concatenate 'string str2
                                 (subseq str1 index1 (1+ index1))))
         (incf index1)
       unless (< index1 (length str1))
       return str2)))

(defun replace-char (string orig replacement)
  "Destructively replaces all occurrences of a character in a string by another one"
  (declare (type string string)
           (type (or character string) orig)
           (type (or character string) replacement))
  ; transform string arguments to characters
  (unless (typep orig 'character)
    (setf orig (character orig)))
  (unless (typep replacement 'character)
    (setf replacement (character replacement)))
  ; maybe there's also a way to do this with a map-like construction
  ; instead of a loop and still be destructive?
  ; using (position orig string :start _) doesn't work because of a bug in ccl
  (loop for i from 0 to (1- (length string))
     when (eq orig (aref string i)) do
       (setf (aref string i) replacement))
  string)

(defun read-file-as-string (pathname)
  (declare (type pathname pathname))
  (let ((os (make-string-output-stream)))
    (with-open-file (is (probe-file pathname))
      (loop for char = (read-char is nil)
            while char
            do (write-char char os)))
    (get-output-stream-string os)))

(defgeneric read-until (string until-part &key start from-end &allow-other-keys)
  (:documentation "Returns part of the string until the until-part is
  encountered."))

(defmethod read-until ((string string) (until-part character) &key (start 0) (from-end nil))
  (subseq string start (position until-part string 
				 :start start :from-end from-end)))

(defmethod read-until ((string string) (until-part string) &key (start 0) (from-end nil))
  (subseq string start (search until-part string 
			       :start1 start :from-end from-end)))

(defun camel-case->lisp (camel-string)
  "Insert - between lowercase and uppercase chars
   and make everything uppercase"
  (declare (string camel-string))
  (let ((*print-pretty* nil))
    (with-output-to-string (result)
      (loop with last-was-lowercase
            for c across camel-string
            when (and last-was-lowercase
                      (upper-case-p c))
            do (princ "-" result)
            if (lower-case-p c)
            do (setf last-was-lowercase t)
            else
            do (setf last-was-lowercase nil)
            do (princ (char-upcase c) result)))))

(defun lisp->camel-case (string &key (from-first t))
  "Remove - between words and make all words uppercase, except the first.
   When from-first is true, the first word is also uppercase."
  (declare (string string))
  (let ((*print-pretty* nil))
    (with-output-to-string (result)
      (loop with last-was-dash
            for c across string
            for i below (length string)
            if (eql c #\-)
            do (setf last-was-dash t)
            else
            do (progn
                 (cond
                  ((and (= i 0) from-first)
                   (princ (char-upcase c) result))
                  (last-was-dash
                   (princ (char-upcase c) result))
                  (t
                   (princ (char-downcase c) result)))
                 (setf last-was-dash nil))))))


;; ############################################################################

(export '(make-random-string split first-word last-word))

(defun make-random-string (&optional (length 30))
  "Generates and returns a random string length LENGTH.  The
string will consist solely of decimal digits and ASCII letters."
  (with-output-to-string (s)
    (dotimes (i length)
      (write-char (ecase (random 5)
                    ((0 1) (code-char (+ #.(char-code #\a) (random 26))))
                    ((2 3) (code-char (+ #.(char-code #\A) (random 26))))
                    ((4) (code-char (+ #.(char-code #\0) (random 10)))))
                  s))))

(defun split (string separator)
  "Splits a string, taking the substrings between the separator.
    E.g. (split 'ab,cd,ef' ',') yields '('ab' 'cd' 'ef')"
  (split-sequence:split-sequence separator string))

(define-compiler-macro split (string separator)
  `(let ((string ,string)
         (separator ,separator))
     (split-sequence:split-sequence separator string)))

(defun first-word (string)
  (let ((index-word-end (search " " string)))
    (subseq string 0 index-word-end)))

(defun last-word (string)
  (let ((index-word-end (search " " string :from-end t)))
    (if index-word-end
      (subseq string (+ 1 index-word-end))
      string)))


;; ############################################################################

(export '(hyphenize))

(defun hyphenize (string-with-spaces)
  (if (find #\space string-with-spaces)
    (string-replace string-with-spaces " " "-")
    string-with-spaces))

;;(hyphenize "the ocean")

;; ############################################################################

(export '(remove-multiple-spaces))

(defun remove-multiple-spaces (string &key (remove-newlines t))
  "Replaces multiple spaces by a single one, including no-break-spaces."
  (if remove-newlines
      (let* ((string-without-newlines (format nil "~{~a~^ ~}"
                                              (split-sequence:split-sequence #\Newline string :remove-empty-subseqs t)))
             (string-without-no-break-spaces (format nil "~{~a~^ ~}"
                                                     (split-sequence:split-sequence #+LISPWORKS #\No-Break-Space #+CCL #\No-Break_Space
                                                                                    string-without-newlines :remove-empty-subseqs t))))
        (format nil "~{~a~^ ~}" (split-sequence:split-sequence #\Space string-without-no-break-spaces :remove-empty-subseqs t)))
      (let ((string-without-no-break-spaces (format nil "~{~a~^ ~}"
                                                    (split-sequence:split-sequence #+LISPWORKS #\No-Break-Space #+CCL #\No-Break_Space
                                                                                   string :remove-empty-subseqs t))))
        (format nil "~{~a~^ ~}" (split-sequence:split-sequence #\Space string-without-no-break-spaces :remove-empty-subseqs t)))))


;; ############################################################################

(export '(variablifly))

(defun variablify (symbol &key (package *package*))
  "Turn a symbol into a variable if it isn't one yet."
  (if (variable-p symbol)
    symbol
    (intern (format nil "?~a" symbol) package)))