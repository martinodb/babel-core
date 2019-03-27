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
;;; 
;;; File: monitors.asd
;;;
;;; General debugging and monitoring mechanisms for the babel framework
;;;

(in-package :asdf)

(defsystem :monitors
  :description "General debugging and monitoring mechanisms for the babel framework."
  :depends-on (:utils)
  :components 
  ((:file "package")
   (:file "base" :depends-on ("package"))
   (:file "monitors" :depends-on ("base"))
   (:file "trace-monitor" :depends-on ("monitors"))
   (:file "data-monitors" :depends-on ("monitors"))
   (:file "plot-monitors" :depends-on ("data-monitors"))
   (:file "alist-monitors" :depends-on ("plot-monitors"))
   (:file "3d-monitors" :depends-on ("alist-monitors"))
   ))

