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
;;;;
;;;; init-babel.lisp
;;;;
;;;; Registers all Babel asdf systems
;;;;
;;;; Before you can work with Babel, you have to evaluate
;;;;  (load "/your/path/to/Babel2/init-babel.lisp")
;;;;
;;;; Tip: add this command in your lisp system's init file.
;;;;

(in-package :cl-user)

;; ############################################################################
;; asdf system registration
;; ----------------------------------------------------------------------------

(export '(babel-pathname))

;;; The root path of the Babel system. 
(defparameter *babel-path*
  (make-pathname :directory (pathname-directory (or *load-truename*
						    *compile-file-truename*))))

(defun babel-pathname (&key directory name type)
  "Generates a pathname for a file relative to BABEL's root directory."
  (merge-pathnames  (make-pathname :directory  (cons :relative directory) 
				   :name name :type type)
		    *babel-path*))

(ensure-directories-exist (babel-pathname :directory '(".tmp")))

(asdf:initialize-source-registry `(:source-registry
                                   (:tree ,(babel-pathname))
                                   :inherit-configuration))

(format t "~&~%* Initializing BABEL.")
(format t "~%  The BABEL path is: ~a" (directory-namestring *babel-path*))

;; put's the feature :hunchentoot-available-on-this-platform on
;; *features* except in some cases
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (or (and (find :sbcl *features*) (find :sb-thread *features*))
	    (find :cmucl *features*)
	    (find :openmcl *features*) (find :mcl *features*)
	    (find :lispworks *features*)
            (find :allegro-cl-enterprise *features*))
    (pushnew :hunchentoot-available-on-this-platform *features*)))

;; we don't need ssl for hunchentoot
(pushnew :hunchentoot-no-ssl *features*)

;; when t, then the web-interface is automatically started upon compiling
(defvar *automatically-start-web-interface* t)

;; in some lisps *print-pretty* is t by default, that means everything
;; passed to format, print, mkstr, etc or everything returned from an
;; evaluation is pprinted. The monitors and other printing mechanisms
;; in Babel2 are actually very good in deciding what to pprint and
;; what not so your Babel2 experience is much greater when it is off
;; by default (and of course you can still use pprint).

(setf *print-pretty* nil)
(setf *print-circle* nil)

;; the test framework needs to be loaded before other systems are loaded 
;; because it adds asdf methods that have to be there before other systems
;; are loaded

(asdf:operate 'asdf:load-op :test-framework :verbose nil)

;; ccl by default creates a thread with a copy of the global random
;; state and gensym counter for every evaluation from an Emacs
;; buffer. At the beginning of every swank thread, the code below
;; creates a new random state and copies the global gensym
;; counters. To make sure that these changes end up in the right
;; threads, we hook into asdf:perform
#+ccl(defmethod asdf:perform :before ((operation asdf:load-op)
                                      (system asdf:system))
       (when (find-package :swank-backend)
         (let ((gsc (list *gensym-counter*)))
           (setf (get (read-from-string "swank-backend:spawn")
                      (read-from-string "swank-backend::implementation")) 
                 #'(lambda (fun &key name)
                     (ccl:process-run-function 
                      (or name "Anonymous (Swank)")
                      (lambda () 
                        (setq *random-state* (make-random-state t))
                        (setq *gensym-counter* (car gsc))
                        (funcall fun)
                        (setf (car gsc) *gensym-counter*))))))))

(let ((init-babel-user (babel-pathname :name "init-babel-user" :type "lisp")))
  (when (probe-file init-babel-user)
    (load init-babel-user)))



