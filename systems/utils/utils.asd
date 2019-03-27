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
;;;; Definition of the fcg-utils asdf system

(in-package :asdf)

(defsystem :utils
  :description "Basic utilities"
  :depends-on (:split-sequence :cl-ppcre :closer-mop :s-dot :cl-who :test-framework)
  :components 
  ((:file "package")
   (:file "pop" :depends-on ("package"))
   (:file "common-babel-symbols" :depends-on ("package"))
   (:file "owner" :depends-on ("common-babel-symbols"))
   (:file "copy-object" :depends-on ("owner"))
   (:file "math" :depends-on ("package"))
   (:file "lists-and-sets" :depends-on ("math" "package"))
   (:file "blackboard" :depends-on ("lists-and-sets" "copy-object" "package"))
   (:file "relation" :depends-on ("package" "copy-object" "lists-and-sets" "blackboard"))
   (:file "configuration" :depends-on ("package"))
   (:file "symbols-and-strings" :depends-on ("package"))
   (:file "make-new-word" :depends-on ("package"))
   (:file "tree" :depends-on ("package"))
   (:file "queue" :depends-on ("package"))
   (:file "misc-utils" :depends-on ("lists-and-sets" "symbols-and-strings"))
   (:file "shell" :depends-on ("package"))
   (:file "slist" :depends-on ("copy-object"))
   (:file "test-distribution" :depends-on ("package"))
   (:file "timer" :depends-on ("package"))
   (:file "types" :depends-on ("package"))
   (:file "write-to-latex" :depends-on ("package"))
   (:file "clos" :depends-on ("package"))
   (:file "event-dispatcher")
   (:file "ssh-scp")
   (:file "streams")
   (:module tests
    :depends-on ("lists-and-sets" "configuration" "tree" "relation")
    :components 
    ((:file "test-utils")
     (:file "test-relations")
     (:file "test-configuration")))))


