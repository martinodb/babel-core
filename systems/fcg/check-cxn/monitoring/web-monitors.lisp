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
;; Event-handler to output the results of the check-cxn-tests

(in-package :fcg)

(define-event-handler (trace-fcg-processing-level check-cxns-done)
  (let ((report-items (get-report-items report)))
    (when (and (> (length report-items) 0) (item (first report-items)))
      (add-element `((p) ,(string-append "<p style='color:darkorange'><b>Warning: " (string (description report)) "</b></p>")))
      (loop for report-item in report-items
         do (add-element `((p) ,(message report-item) ,(html-pprint (item report-item))))))))