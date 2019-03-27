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

(in-package :tasks-and-processes)

(export '(task-w-learning
          process-w-learning
          process-result-w-learning))

;; ############################################################################
;; process-result-w-learning
;; ----------------------------------------------------------------------------

(defclass process-result-w-learning (process-result object-w-learning) ())

(defmethod add-problem ((result process-result-w-learning)
                        problem &key &allow-other-keys)
  (call-next-method)
  (add-problem (process result) problem))

(defmethod get-diagnostics ((result process-result-w-learning)
                            &key (recursive t) &allow-other-keys)
  (append (diagnostics result)
          (when recursive (get-diagnostics (process result)))))

(defmethod get-repairs ((result process-result-w-learning)
                        &key (recursive t) &allow-other-keys)
  (append (call-next-method)
          (when recursive (get-repairs (process result)))))

(defmethod restart-object ((result process-result-w-learning)
                           (restart-data t)
                           &key process-label &allow-other-keys)
  (restart-object (process result) restart-data :process-label process-label))

;; ############################################################################
;; process-w-learning
;; ----------------------------------------------------------------------------

(defclass process-w-learning (process object-w-learning)
  ())

(defmethod add-problem ((process process-w-learning)
                        problem &key &allow-other-keys)
  (call-next-method)
  (add-problem (task process) problem))

(defmethod get-diagnostics ((process process-w-learning)
                            &key (recursive t) &allow-other-keys)
  (append (diagnostics process)
          (when recursive (get-diagnostics (task process)))))

(defmethod get-repairs ((process process-w-learning)
                        &key (recursive t) &allow-other-keys)
  (append (call-next-method)
          (when recursive (get-repairs (task process)))))

(defmethod restart-object ((process process-w-learning)
                           restart-data
                           &key process-label &allow-other-keys)
  (declare (ignore restart-data))
  (restart-process (task process) process
                   (or process-label (label process))))

;; ############################################################################
;; task-w-learning
;; ----------------------------------------------------------------------------

(defclass task-w-learning (task object-w-learning)
  ((configuration :initform '((:run-task-process-class . process-w-learning)))))

