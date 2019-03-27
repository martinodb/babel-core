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
;;; A web interface to the rule sets of an agent

(in-package :asdf)

(defsystem :web-interface
  :description "A web interface to the rule sets of an agent."
  :depends-on (:utils :hunchentoot :ht-simple-ajax :monitors :bordeaux-threads)
  :components 
  ((:file "package")
   (:file "render-xml" :depends-on ("package"))
   (:file "web-interface" :depends-on ("render-xml"))
   (:file "html-utils" :depends-on ("web-interface"))
   (:file "dot" :depends-on ("web-interface"))
   (:file "draw-predicate-network" :depends-on ("dot"))
   (:file "static-flash-network" :depends-on ("web-interface"))
   (:file "presentation" :depends-on ("static-flash-network"))
   (:file "3d-svg" :depends-on ("presentation"))
   (:file "class-diagram" :depends-on ("3d-svg"))))