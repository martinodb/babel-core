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

(export '(trace-interaction))

(define-monitor trace-interaction
                :documentation "Displays information about an interaction in the web interface")

;; ============================================================================
;; interaction-started/interaction-finished
;; ============================================================================

(define-event-handler (trace-interaction interaction-started)
  (add-element `((h2) ,(format nil "Interaction ~a"
                               interaction-number))))

(define-event-handler (trace-interaction interaction-finished)
  (add-element
   `((p) "Interaction " 
     ((b) ,(if (communicated-successfully interaction)
             "succeeded" "failed")))))


