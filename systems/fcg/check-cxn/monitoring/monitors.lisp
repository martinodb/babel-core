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
;;; This file creates a monitor for tracing results from construction
;;; -checking into the repl.

(in-package :fcg)

(export '(check-cxns check-cxns-done))

;; Define the monitor for tracing check-cxn messages in the web-interface / repl
;; -----------------------------------------------------------------------------

(define-monitor check-cxns
    :documentation "Displays information about the integrity of an FCG-construction in the web-interface and in the repl."
    :class 'trace-monitor)

(define-event check-cxns-done (report report))

; Now available via: (activate-monitor check-cxns)

(define-event-handler (check-cxns cxn-added)
  (format (monitor-stream monitor)
            "~&----~&FCG-STRUCTURE-TESTS: Checked construction ~a for structural integrity with the following tests: ~a" (name construction) (or (get-configuration construction-inventory :check-cxn-tests) *check-cxn-tests*))
    (loop for test in (or (get-configuration construction-inventory :check-cxn-tests) *check-cxn-tests*)
       when test
       do (notify check-cxns-done (check-cxn construction test))))

;; Event-handler to output the results of the check-cxn-tests
(define-event-handler (check-cxns check-cxns-done)
  (let ((report-items (get-report-items report)))
    (when (and (> (length report-items) 0) (item (first report-items)))
      (format (monitor-stream monitor) "~& + Warning: ~a" (string (description report)))
      (loop for report-item in report-items
         do (format (monitor-stream monitor) "~&   - ~a: ~a" (item report-item) (message report-item))))))
