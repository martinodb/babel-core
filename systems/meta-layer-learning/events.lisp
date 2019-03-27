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

(export '(trace-learning trace-learning-verbose))

;; ############################################################################
;; Monitors for tracing learning mechanisms
;; ----------------------------------------------------------------------------

(export '(repairing-started repairing-finished repaired
          diagnostic-started diagnostic
          diagnostic-returned-problems
          diagnosed-problems
          repair-strategy
          repaired-thing))

;; diagnostic started
(define-event diagnostic-started (diagnostic t)(id t))

;; diagnostic returned problem
(define-event diagnostic-returned-problems (diagnostic t) (problems list))

;; when diagnosed problems
(define-event diagnosed-problems (diagnostic  t)(problems list))

;; when repairing started
(define-event repairing-started (repair-strategy t) (problem t) 
	      (repaired-thing t))

;; when repairing finished
(define-event repairing-finished (repair-strategy t) (repaired t) 
	      (repaired-thing t))

(define-event repaired (repair-strategy t) (repaired-thing t))
