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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate-and-test renderer                                                                                                 ;;                  
;;                                                                                                                            ;;
;; The render-mode :generate-and test  implements rendering as a search process, in which                                     ;;
;; the nodes are render states. Each render state has three slots: used-string-constraints,                                   ;;
;; containing an ordered list of string constraints (e.g. (string ?x "y")) being build up and representing the utterance,     ;;
;; remaining-string-constraints, containing string constraints that still need to be added to the                             ;;
;; utterance, and ordering-constraints, containing all ordering constraints (e.g. (meets ?x ?y)).                             ;;
;; At each step in the search process, a string constraint is taken from remaining-string-constraints and added               ;;
;; to the right of used-string-constraints (generate step), and this new state is tested against the constraints (test step). ;;
;; If the utterance-so-far satisfies the constraints, this state is added to the queue. A state is considered a solution      ;;
;; if there are no more remaining-string-constraints. The strings (third element) of the used-string-constraints are          ;;
;; then considered the rendered utterance.                                                                                    ;;
;;                                                                                                                            ;;
;; For implementing a new ordering constraint, just implement a function with the name of the constraint, and as that         ;;
;; arguments the arguments of the constraint + a list of string constraints. Ensure that the function returns t if the        ;;
;; list of string constraints satisfies the ordering constrint, nil otherwise. See below e.g. meets or precedes.              ;;
;;                                                                                                                            ;; 
;; For efficiency reasons, try to to implement the constraint is such a way that it failes soon enougth. For example,         ;;
;; consider (meets ?x ?y). When ?y is found in used-string-constraints and ?x not, the meets function can already safely      ;;
;; return nil, as it will never be a solution. No need to wait until both ?x and ?y have been found.                          ;;
;;                                                                                                                            ;;
;; Ordering constraints that contain units not present in any string constraint are discarded at the beginning.               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

(export '(meets precedes first-el))

(defmethod render ((cfs coupled-feature-structure) (mode (eql :generate-and-test)) &key &allow-other-keys)
  (render (extract-forms (left-pole-structure cfs)) :generate-and-test))

;;;;;;;;;;
;; Core ;;
;;;;;;;;;;

(defclass render-state ()
  ((used-string-constraints
    :type list :initarg :used-string-constraints 
    :accessor used-string-constraints
    :initform nil
    :documentation "A list of string constraints representing the rendered utterance so far")
   (remaining-string-constraints
    :type list :initarg :remaining-string-constraints 
    :accessor remaining-string-constraints
    :initform nil
    :documentation "A list of string constraints that still need to be added to the utterance")
   (ordering-constraints 
    :type list :initarg :ordering-constraints 
    :accessor ordering-constraints
    :initform nil
    :documentation "A list of ordering constraints that the final utterance should satisfy")))


(defmethod render ((form-constraints list) (mode (eql :generate-and-test)) &key &allow-other-keys)
  "Returns a list of strings satisfying the list-of-form-constraints"
  (let* ((string-constraints (remove-if-not #'stringp form-constraints :key #'third))
         (ordering-constraints (filter-by-string-constraints (remove-if #'stringp form-constraints :key #'third)
                                                             string-constraints))
        (queue (list (make-instance 'render-state
                                    :used-string-constraints nil
                                    :remaining-string-constraints (shuffle string-constraints)
                                    :ordering-constraints ordering-constraints))))
    (loop while queue
          for current-state = (pop queue)
          for all-new-states = (generate-render-states current-state)
          for valid-new-states = (test-render-states all-new-states)
          if (find nil valid-new-states :key #'remaining-string-constraints)
          return (render (find nil valid-new-states :key #'remaining-string-constraints) :generate-and-test)
          else
          do
          (setf queue (append valid-new-states queue)))))

(defmethod render ((state render-state) (mode (eql :generate-and-test)) &key &allow-other-keys)
  "Returns the list of strings represented in a render-state"
  (mapcar #'third (used-string-constraints state)))

(defun generate-render-states (current-state)
  "Generates new render states adding an extra string constraint to used-string-constraints"
  (loop for rsc in (remaining-string-constraints current-state)
                                 collect (make-instance 'render-state
                                                        :used-string-constraints
                                                        (append (used-string-constraints current-state) (list rsc))
                                                        :remaining-string-constraints
                                                        (remove rsc (remaining-string-constraints current-state) :test #'equal)
                                                        :ordering-constraints
                                                        (ordering-constraints current-state))))


(defun test-render-states (list-of-states)
  "Tests each state in list-of-states and returns a list of states that passed the test"
  (loop for state in list-of-states
        when (test-render-state state)
        collect state))

(defun test-render-state (state)
  "Tests the used-string-constraints of a state against the ordering-constraints and returns t if it passes"
  (let ((used-string-constraints (used-string-constraints state))
        (ordering-constraints (ordering-constraints state)))
    (loop with solution = t
          for oc in ordering-constraints
          unless (apply (first oc) (append (rest oc) (list used-string-constraints)))
          do (setf solution nil)
          finally (return solution))))

(defun filter-by-string-constraints (ordering-constraints string-constraints)
  "Returns only those ordering constraints of which the units occur in
string constraints. E.g. discards (meets ?X ?Y), where there is no
string constraint with the variable ?Y."
  (loop for oc in ordering-constraints
        unless (loop for unit in (rest oc)
                     unless (find unit string-constraints :key #'second :test #'equal)
                     return t)
        collect oc))

;; (filter-by-string-constraints '((first ?x) (meets ?x ?y) (precedes ?x ?z)) '((string ?x "x") (string ?y "y")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ordering Constraints ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun meets (el-1 el-2 string-constraints)
  "If el-1 and el-2 are in string-constraints el-1 must be immediately left-adjacent to el-2"
  (let ((index-el-1 (position el-1 string-constraints :key #'second :test #'equal))
        (index-el-2 (position el-2 string-constraints :key #'second :test #'equal)))
    (cond
     ;; for efficiency
     ((and (null index-el-1)
           index-el-2)
      nil)
     ;; real testing
     ((or (null index-el-1) (null index-el-2))
      t)
     ((= index-el-1 (- index-el-2 1))
      t)
     (t
      nil))))

;; (meets '?red-1 '?cube-1 '((string ?the-1"the") (string ?red-1 "red") (string ?cube-1 "cube")))
;; (meets '?cube-1 '?red-1 '((string ?the-1 "the") (string ?red-1 "red") (string ?cube-1 "cube")))
;; (meets '?the-1 '?cube-1 '((string ?the-1 "the") (string ?red-1 "red") (string ?cube-1 "cube")))

(defun precedes (el-1 el-2 string-constraints)
  "If el-1 and el-2 are in string-constraints el-1 must be occur earlier than el-2"
  (let ((index-el-1 (position el-1 string-constraints :key #'second :test #'equal))
        (index-el-2 (position el-2 string-constraints :key #'second :test #'equal)))
    (cond 
     ;; for efficiency
     ((and (null index-el-1)
           index-el-2)
      nil)
     ;; real testing
     ((or (null index-el-1) (null index-el-2))
      t)
     ((< index-el-1 index-el-2)
      t)
     (t
      nil))))

;; (precedes '?red-1 '?cube-1 '((string ?the-1"the") (string ?red-1 "red") (string ?cube-1 "cube")))
;; (precedes '?cube-1 '?red-1 '((string ?the-1 "the") (string ?red-1 "red") (string ?cube-1 "cube")))
;; (precedes '?the-1 '?cube-1 '((string ?the-1 "the") (string ?red-1 "red") (string ?cube-1 "cube")))

(defun first-el (el string-constraints)
  "el should be the first element in string-constraints"
  (let ((index-el (position el string-constraints :key #'second :test #'equal)))
    (if (and index-el (= index-el 0))
          t
          nil)))

;; (first-el '?the-1  '((string ?the-1"the") (string ?red-1 "red") (string ?cube-1 "cube")))
;; (first-el '?red-1  '((string ?the-1"the") (string ?red-1 "red") (string ?cube-1 "cube")))
;; (first-el '?cube-1  '((string ?the-1"the") (string ?red-1 "red") (string ?cube-1 "cube")))


