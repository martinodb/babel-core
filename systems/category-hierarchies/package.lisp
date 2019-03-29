
(in-package :common-lisp-user)

(defpackage :type-hierarchies
  (:use :common-lisp :monitors :utils :test-framework :fcg :s-dot :graph-utils
	#+:hunchentoot-available-on-this-platform :web-interface)
  (:shadow "UNIFY" "WEIGHT" "ADD-NODE" "LEAF?")
  (:documentation "Package for type hierarchy support in FCG")
  (:import-from :cl-user))

(pushnew :type-hierarchies *features*)