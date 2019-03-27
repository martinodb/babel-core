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

;; ############################################################################

(export '(action agent-id recipient-ids all-agents))

(defclass action ()
  ((agent-id
    :type (or symbol fixnum)
    :initform nil
    :initarg :agent-id
    :accessor agent-id
    :documentation "The id of the agent that does the action")
   (recipient-ids
    :type list
    :initform '(all-agents)
    :initarg :recipient-ids
    :accessor recipient-ids
    :documentation "The ids of the agents that the action is directed to. 
      By default, the action is performed to all interacting agents.")))

(defmethod print-object ((action action) stream)
  (if *print-pretty* 
    (pprint-logical-block (stream nil)
      (format stream "<action ~(~a~) :~:_ agent-id: ~a,~:_ recipient-ids: ~w>" 
              (class-string action) (agent-id action) (recipient-ids action)))
    (format stream "<action ~(~a~)>" (class-name (class-of action)))))

;; ----------------------------------------------------------------------------
;; Some ready made actions - (you can use them but you do not have to)
;; ----------------------------------------------------------------------------

(export '(no-action speak-action point-action
                    signal-failure-action signal-failure-and-point-action
                    signal-success-action))

;; ----------------------------------------------------------------------------

(defclass no-action (action) ()
  (:documentation "An action that does nothing. 
Indicates that an agent waits or thinks that the interaction is finished."))

(defmethod print-object ((action no-action) stream)
  (if *print-pretty* 
    (pprint-logical-block (stream nil)
      (format stream "<~a:~:_ " (class-string action))
      (call-next-method) (format stream ">"))
    (call-next-method)))

;; ----------------------------------------------------------------------------

(defclass speak-action (action)
  ((utterance 
    :initarg :utterance :accessor utterance))
  (:documentation "Action the speaker could produce at the end of his
  turn."))

(defmethod print-object ((action speak-action) stream)
  (pprint-logical-block (stream nil)
    (format stream "<speak-action:~:_ ") 
    (call-next-method) 
    (format stream "~:_ utterance: ~a>" (utterance action))))

;; ----------------------------------------------------------------------------

(defclass point-action (action)
  ((thing :initarg :thing :accessor thing))
  (:documentation "Point to something in the world."))

(defmethod print-object ((action point-action) stream)
  (pprint-logical-block (stream nil)
    (format stream "<~a:~:_ " (class-string action)) 
    (call-next-method) 
    (format stream "thing: ~a>" (thing action))))

;; ----------------------------------------------------------------------------

(defclass signal-failure-action (action)
  ()
  (:documentation "Signal communicative failure to an interlocutor. Like shaking head."))

;; ----------------------------------------------------------------------------

(defclass signal-failure-and-point-action (point-action)
  ()
  (:documentation "Signal communicative failure and point to the intended thing."))

;; ----------------------------------------------------------------------------

(defclass signal-success-action (action)
  ()
  (:documentation "signal communicative success to an interlocutor. Like nodding."))

