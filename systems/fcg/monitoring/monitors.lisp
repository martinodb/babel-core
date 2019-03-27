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

(in-package :fcg)

(define-monitor fcg-warnings
    :class 'trace-monitor
    :documentation "This monitor collects all warnings thrown by fcg. Turn this monitor on when you want to debug your grammar.")

(define-event-handler (fcg-warnings warn-unable-to-determine-domain)
  (format-monitor "~%WARNING: HANDLE-J-UNIT -- could not detect domain of structure, using 'subunits'.~%  STRUCTURE: ~A"
		  structure))

(define-event-handler (fcg-warnings warn-unable-to-merge-tag-value)
  (format-monitor "~%WARNING: HANDLE-J-UNIT -- could not merge tag-value into unit.~%  J-UNIT: ~A~%  TAG: ~A~%  TAG-VALUE: ~A~%  UNIT-FEATURES: ~A"
		  j-unit tag-variable tag-value 
                  (unit-features unit)))

(define-event-handler (fcg-warnings warn-tag-variable-unbound)
  (format-monitor "~%WARNING: HANDLE-J-UNIT -- tag-variable unbound.~%  J-UNIT:~A~%  TAG: ~A~%"
		  j-unit tag-variable))

(define-event-handler (fcg-warnings warn-hierarchy-in-structure-broken)
  (format-monitor "~%WARNING: HANDLE-J-UNIT -- cannot handle J-unit, would break hierarchical structure.~%  J-UNIT: ~A~%  PATTERN: ~A~%  BINDINGS: ~A"
		  j-unit pattern bindings))

#-:hunchentoot-available-on-this-platform
(progn
  (macrolet ((define-web-monitor-substitute (id)
	       `(progn
		  (define-monitor ,id :documentation "not available.")
		  (defmethod monitors::activate-monitor-method ((id (eql ',id)) &optional active)
		    (declare (ignore active))
		    (format t "~% !!! The web-interface is not available on this machine.~
                            ~% !!! Monitor ~a thus doesn't work.~%" ',id)))))
    (define-web-monitor-substitute trace-fcg-processing-level-search-process)
    (define-web-monitor-substitute trace-fcg-processing-level))

  (defun clear-page () "web-interface not available"))


;; we export the web monitors from here so that they are still there
;; when the web interface is not loaded
(export '(trace-fcg-processing-level-search-process trace-fcg-processing-level trace-fcg trace-fcg-search-process))




  
				   
