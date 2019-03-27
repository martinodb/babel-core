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

(defsystem :network
  :description "Basic support for the datastructure network"
  :depends-on (:s-dot :test-framework :utils  :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :components 
  ((:file "package")
   (:file "network" :depends-on ("package"))
   (:file "drawing" :depends-on ("network"))
   #+:hunchentoot-available-on-this-platform 
   (:file "html" :depends-on ("network" "drawing"))
   
   ;; (:file "rule-network" :depends-on ("drawing" "network"))
   ;;(:file "rule-network-application" :depends-on '("rule-network"))
   ))


