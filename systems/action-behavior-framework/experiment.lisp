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
;; experiment
;; ============================================================================

(export '(action-experiment))

(defclass action-experiment (experiment)
  ((world :initform (make-instance 'action-world))))

;; ----------------------------------------------------------------------------

(defmethod begin-interaction ((interaction interaction)
                              &rest parameters
                              &key)
  "experiment begin-interaction calls begin-interaction on world
   and interacting agents"
  (loop for agent in (interacting-agents interaction)
        do (apply 'begin-interaction agent parameters)))

(defmethod begin-interaction ((experiment experiment)
                              &rest parameters
                              &key)
  "experiment begin-interaction calls begin-interaction on world
   and interacting agents"
  (apply 'begin-interaction (world experiment) parameters)
  (apply 'begin-interaction (car (interactions experiment))
         parameters))

;; ----------------------------------------------------------------------------

(defmethod finish-interaction ((interaction interaction)
                               &rest parameters
                               &key)
  "experiment finish-interaction calls finish-interaction on world
   and interacting agents"
  (loop for agent in (interacting-agents interaction)
        do (apply 'finish-interaction agent parameters)))

(defmethod finish-interaction ((experiment experiment)
                               
                               &rest parameters
                               &key)
  "experiment finish-interaction calls finish-interaction on world
   and interacting agents"
  (apply 'finish-interaction (world experiment) parameters)
  (apply 'finish-interaction (car (interactions experiment)) parameters))

;; ----------------------------------------------------------------------------

(defmethod interact ((experiment action-experiment)
                     (interaction interaction) &key &allow-other-keys)
  (begin-interaction experiment)
  (loop with at-least-one-agent-returned-an-action = nil
        do
        (setf at-least-one-agent-returned-an-action nil)
        (loop for agent in (interacting-agents interaction)
              do
              (notify run-agent-started agent (world experiment)
                      interaction experiment)
              (let ((action (or (run-agent agent (world experiment)
                                           interaction experiment)
                                (make-instance 'no-action))))
                (update-world (world experiment) action)
                (unless (typep action 'no-action)
                  (setf at-least-one-agent-returned-an-action t))
                (notify run-agent-finished agent (world experiment)
                        interaction experiment
                        action)))
        while at-least-one-agent-returned-an-action)
  (loop for agent in (interacting-agents interaction)
        do
        (notify consolidation-started agent)
        (consolidate-agent agent (world experiment))
        (notify consolidation-ended agent))
  (finish-interaction experiment))
