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

(in-package :meta-layer-learning)

(export '(trace-learning trace-learning-verbose repair-returned-fixes))

;; ############################################################################
;; Monitors for tracing learning mechanisms
;; ----------------------------------------------------------------------------

(define-monitor trace-learning :class 'trace-monitor
		:documentation "Prints information on detected and repaired problems.")

(define-monitor trace-learning-verbose :class 'trace-monitor
		:documentation "In addition to monitor trace-learning, ~
                  also prints which learning mechanisms are run")

(define-event-handler ((trace-learning-verbose trace-learning) diagnostic-started)
  (format (monitor-stream monitor) "~%+ Running diagnostic ~(~a~) for ~(~a~)" 
	  (type-of diagnostic) id))

(define-event-handler ((trace-learning-verbose trace-learning)
                       diagnostic-returned-problems)
  (print-with-overline monitor #\+ 
		       (format nil "+ Diagnostic ~(~a~) detected problems:"
			       (type-of diagnostic)))
  (format (monitor-stream monitor) "~%   ~:w" problems))
  
(define-event-handler ((trace-learning-verbose trace-learning) repairing-started)
  (format (monitor-stream monitor)
          "~%+ Running repair strategy ~(~a~) for ~(~a~)~%   on problem ~a."
	  (type-of repair-strategy) (type-of repaired-thing) problem))

(define-event repair-returned-fixes (repair repair) (fix fix))

(define-event-handler ((trace-learning-verbose trace-learning)
                       repair-returned-fixes)
  (print-with-overline monitor #\+ 
		       (format nil "+ Repair ~(~a~) returned fix"
			       (type-of repair)))
  (format (monitor-stream monitor) "~%   ~:w" fix))


