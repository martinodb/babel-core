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

(defsystem :meta-layer-learning
  :description "Compiles all files needed to use meta-layer learning
  such as diagnostics and repairs"
  :depends-on (:test-framework :utils :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
  ((:file "package")
   (:file "notify-learning")
   (:file "diagnostic-problem-repair-fix")
   (:file "object-w-learning")
   (:file "events")
   (:file "monitors")
   #+:hunchentoot-available-on-this-platform
   (:file "html")
   (:file "test-meta-layer-learning")))


