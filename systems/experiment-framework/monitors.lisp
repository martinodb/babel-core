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
;; trace-interaction monitor:
;; ----------------------------------------------------------------------------

(export '(trace-interaction-in-repl
	  trace-experiment-in-repl
	  print-a-dot-for-each-interaction))

(define-monitor trace-interaction-in-repl
                :class 'trace-monitor
                :documentation "Prints some high-level information about each interaction")

(define-event-handler (trace-interaction-in-repl interaction-started)
  (format (monitor-stream monitor) "~%")
  (print-with-overline monitor #\# 
		       (format nil "# Started interaction ~a."
			           interaction-number)))

(define-event-handler (trace-interaction-in-repl interacting-agents-determined)
  (format (monitor-stream monitor) "~%# Interacting agents: ~a" 
	  (interacting-agents experiment)))

;; ############################################################################
;; trace-experiment-in-repl monitor:
;; ----------------------------------------------------------------------------

(define-monitor trace-experiment-in-repl :class 'trace-monitor
		:documentation "Prints some information about series and batches.")

(define-event-handler (trace-experiment-in-repl interaction-started)
  (when (= (mod (interaction-number interaction) 100) 0)
    (format-monitor "~%# Interaction ~a." (interaction-number interaction))))

(define-event-handler (trace-experiment-in-repl series-finished)
  (format-monitor "~%# Finished series ~a." series-number))

(define-event-handler (trace-experiment-in-repl batch-finished)
  (print-with-overline monitor #\# "# Finished batch."))

;; ############################################################################
;; print-a-dot-for-each-interaction monitor:
;; ----------------------------------------------------------------------------

(define-monitor print-a-dot-for-each-interaction 
                :documentation "Prints a '.' for each interaction")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (format t "."))

;; ############################################################################
;; Data monitors
;; ----------------------------------------------------------------------------

(export '(default-record-communicative-success))

(define-monitor default-record-communicative-success 
                :class 'data-recorder
                :documentation "records the communicative success
	                        of the first of the interacting agents.")

(define-event-handler (default-record-communicative-success interaction-finished)
  (record-value monitor 
                (if (communicated-successfully interaction)
                  1 0)))

;; ############################################################################
