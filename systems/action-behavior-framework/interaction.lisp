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

(in-package :action-behavior-framework)

(export '(begin-interaction))

;; ----------------------------------------------------------------------------
;; begin-interaction

(export '(begin-interaction))

(defgeneric begin-interaction (thing &key)
  (:documentation "initializes thing for interaction"))

(defmethod begin-interaction (thing &key &allow-other-keys)
  (declare (ignore thing)))

;; ----------------------------------------------------------------------------
;; finish-interaction

(export '(finish-interaction))

(defgeneric finish-interaction (thing &key)
  (:documentation "finish thing for interaction"))

(defmethod finish-interaction (thing &key &allow-other-keys)
  (declare (ignore thing)))

