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
;; ************** Test Distribution **************
;; ** Simon Pauw
;; ***********************************************
;; (test-distribution zero-place-function      => the function that generates a distribution
;;                    number-of-trials         => the amount of trials
;;                    :test #'equality-test)   => the equality test

;; ******************* Example: ******************
;; (test-distribution #'(lambda () (random 5)) 1000 :test #'eq)
;; 0 => 215
;; 1 => 195
;; 2 => 168
;; 3 => 210
;; 4 => 207
;; ***********************************************


(in-package :utils)

(export '(test-distribution))


(defun test-distribution (fun trials &key (test #'eq))
  (let ((hash (make-hash-table :test test)))
    (dotimes (i trials)
      (inc-hash-value (funcall fun) hash))
    (maphash #'(lambda (x y) (format *standard-output* "~a => ~a~%" x y))
             hash)
    hash))

(defun inc-hash-value (item hash-table)
  (if (gethash item hash-table)
    (incf (gethash item hash-table))
    (setf (gethash item hash-table) 0)))
