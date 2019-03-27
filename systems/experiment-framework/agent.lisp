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

(in-package :experiment-framework)

;; ############################################################################
;; agent
;; ----------------------------------------------------------------------------

(export '(agent
          discourse-role utterance
          communicated-successfully
          speakerp hearerp))

(defclass agent (configuration blackboard)
  ((experiment :initarg :experiment :accessor experiment :initform nil
               :documentation "The experiment this agent is part of")
   (id
    :documentation "The unique identifier of this agent."
    :type (or symbol fixnum) :initarg :id :initform (make-id "AGENT") :reader id)    
   (discourse-role 
    :documentation "A symbol designating the role the agent plays in
    the game. e.g. unknown, speaker, hearer. You have to set this slot
    yourself in method determine-interacting-agents."
    :type symbol :initform 'not-set :accessor discourse-role)
   (utterance 
    :documentation "The utterance that either a speaker produced or a
    hearer received. You have to set this slot yourself."
    :type t :initarg :utterance :initform nil :accessor utterance)
   (communicated-successfully 
    :documentation "Whether the agent experienced success."
    :initform nil :accessor communicated-successfully))
  (:documentation "Base class for all agents"))

;; ----------------------------------------------------------------------------
;; speakerp and hearerp

(defgeneric speakerp (agent)
  (:documentation "Predicate to check if the agent is the speaker"))

(defmethod speakerp ((agent agent))
  (eql (discourse-role agent) 'speaker))

(defgeneric hearerp (agent)
  (:documentation "Predicate to check if the agent is the hearer"))

(defmethod hearerp ((agent agent))
  (eql (discourse-role agent) 'hearer))

;; ----------------------------------------------------------------------------
;; get-configuration

(defmethod get-configuration ((agent agent) key &key &allow-other-keys)
  "get-configuration for a agent tries the experiment if no configuration
   is found in the agent"
  (multiple-value-bind (entry found)
      (call-next-method)
    (if found
      (values entry found)
      (get-configuration (experiment agent) key))))

;; ----------------------------------------------------------------------------
;; print-object

(defmethod print-object ((agent agent) stream)
  (if *print-pretty*
    (pprint-logical-block (stream nil)
      (format stream "<~(~:w~):~:_ id: ~a,~:_ configuration: ~a,~:_" 
              (type-of agent) (id agent) (configuration agent))
      (call-next-method)
      (format stream "~:_ data: ~a>" (data agent)))
    (format stream "<~(~:w~) ~a>" (type-of agent) (id agent))))


