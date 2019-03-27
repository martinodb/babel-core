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

;; ============================================================================
;; world
;; ============================================================================

(export '(action-world))

(defclass action-world (configuration)
  ((actions
    :type list
    :initform nil
    :accessor actions
    :documentation "All actions that were performed during an interaction (latest first)")))

;; ----------------------------------------------------------------------------

(defmethod begin-interaction ((world action-world)
                              &key &allow-other-keys)
  (setf (actions world) nil))

;; ----------------------------------------------------------------------------

(export '(update-world world-updated))

(define-event world-updated (world action-world))

(defgeneric update-world (world action)
  (:documentation "Updates the world dependent on the last action of an agent"))

(defmethod update-world ((world action-world)
                         (action action))
  (push action (actions world))
  (notify world-updated world))

(defmethod update-world ((world action-world)
                         (action (eql nil)))
  (push (make-instance 'no-action) (actions world))
  (notify world-updated world))

;; ----------------------------------------------------------------------------

(defmethod print-object ((world action-world) stream)
  (if *print-pretty* 
    (pprint-logical-block (stream nil)
      (format stream "<world:~:_ actions: ~a,~:_ configuration: ~a>" 
              (actions world) (configuration world)))
    (call-next-method)))
