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

;; expansion-data is used for expanding ++.
;;; You should not immediatly access the global variable but instead
;;; use the interface functions.

(export '(set-fcg-expansion-data get-fcg-expansion-data get-fcg-expansion-data-for-type fcg-expand
          set-fcg-expansion-data-for-type))

;;private: don't access *expansion-data* directly
(defparameter *expansion-data* (make-instance 'blackboard))

;; public interface for expansion-data

(defun set-fcg-expansion-data (expansion-data)
  (setf *expansion-data* expansion-data))

(defun get-fcg-expansion-data ()
  *expansion-data*)

(defun get-fcg-expansion-data-for-type (type)
  (find-data (get-fcg-expansion-data) type))

(defun set-fcg-expansion-data-for-type (type data)
  (set-data (get-fcg-expansion-data) type data))

;; Part of the code is also in
;; systems/fcg/construction-inventories/construction-inventory.lisp. More
;; specifically the slot expansion-data in the abstract class
;; construction-inventory and some setter and getter functions.

(defgeneric fcg-expand (type &key value source bindings merge? &allow-other-keys)
  (:documentation "Expands the given value based on its type. Methods
  should specialize on eql of type. You can use
  get-fcg-expansion-data-for-type to get additional data."))


;;;;; =====================
;;;;; Implementation of ++
;;;;; =====================

(export '++)

(defun get-expansion-type (pattern)
  (declare (list pattern))
  (assert (and (eq (first pattern) '++)
               (= 3 (length pattern)))) ;; should look like (++ type value)
  (second pattern))

(defun get-expansion-value (pattern)
  (declare (list pattern))
  (assert (and (eq (first pattern) '++)
               (= 3 (length pattern)))) ;; should look like (++ type value)
  (third pattern))

(defun unify-expansion (pattern source bindings-list &key cxn-inventory)
  "Expands the value (in pattern) based on its type and unify-includes
it."
  (multiple-value-bind (expanded-structure new-bindings)
              (fcg-expand (get-expansion-type pattern) 
                          :value (get-expansion-value pattern) 
                          :source source
                          :bindings bindings-list
                          :merge? nil)
    (when new-bindings
      (setf bindings-list new-bindings))
    (unify expanded-structure source bindings-list :cxn-inventory cxn-inventory)))

(defun merge-expansion (pattern source bindings &key &allow-other-keys)
  "Expands the value (in pattern) based on its type and merge-includes
it."
  (let ((expanded-structure (fcg-expand (get-expansion-type pattern) 
                                        :value (get-expansion-value pattern) 
                                        :source source
                                        :bindings bindings
                                        :merge? t)))
    (fcg-merge expanded-structure source bindings)))

(defun clean-expansion (pattern bindings)
  ;; cleaning a ++ also entails expanding it! This is required
  ;; because in merging when source = nil merge-fn isn't called (only
  ;; clean-fn)
  (let ((expanded-structure (fcg-expand (get-expansion-type pattern) 
                                        :value (get-expansion-value pattern) 
                                        :source nil
                                        :bindings bindings
                                        :merge? t)))
    (values (remove-special-operators expanded-structure bindings) bindings)))


(add-special-operator (make-instance 'special-operator
                                     :representation '++
                                     :unify-fn #'unify-expansion
                                     :merge-fn #'merge-expansion
                                     :clean-fn #'clean-expansion))

