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

(in-package :common-lisp-user)

(defpackage :type-hierarchies
  (:use :common-lisp :monitors :utils :test-framework :fcg :s-dot :graph-utils
	#+:hunchentoot-available-on-this-platform :web-interface)
  (:shadow "UNIFY" "WEIGHT" "ADD-NODE" "LEAF?")
  (:documentation "Package for type hierarchy support in FCG")
  (:import-from :cl-user))

(pushnew :type-hierarchies *features*)