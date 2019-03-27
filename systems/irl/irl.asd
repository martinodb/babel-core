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

(in-package :asdf)

(defsystem :irl
  :description "Incremental Recruitment Language"
  :depends-on (:test-framework :utils :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
  ((:file "package")
   (:file "entity")
   (:file "primitive")
   (:file "binding")
   (:file "evaluate-primitive")
   (:file "irl-programs")
   (:module chunk-composer
            :serial t
            :components 
            ((:file "chunk")
             (:file "match-chunk")
             (:file "evaluate-chunk")
             (:file "composer")
             (:file "single-topic-composer")
             (:file "multi-topic-composer")
             (:file "learning")))
   (:module monitoring
            :serial t
            :components 
            (#+:hunchentoot-available-on-this-platform (:file "draw-irl-program")
             #+:hunchentoot-available-on-this-platform (:file "html")
             #+:hunchentoot-available-on-this-platform (:file "web-monitors")))
   (:module tests
            :serial t
            :components 
            ((:file "apple-counting-example")
             (:file "test-evaluate-primitive")
             (:file "test-evaluate-irl-program")
             (:file "test-equivalent-irl-programs")
             (:file "test-irl-program-connected")
             (:file "test-expand-chunk")
             (:file "test-match-chunk")
             (:file "test-binding-helpers")))))




;;;;; This cleans the irl directory after it was compiled. Because asdf
;;;;; doesn't recompile files in module b when module a changed, this
;;;;; can help during development

;; (defmethod perform :after ((op load-op) (c (eql (asdf:find-system :irl))))
;;   (let ((*verbose-out* nil))
;;     (run-shell-command (format nil "cd ~a && ../../clean.sh" 
;;                                (component-pathname (asdf:find-system :irl))))))

