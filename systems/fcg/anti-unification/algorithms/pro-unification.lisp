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

(defun apply-pro-unification (cxn ts direction)
  "returns new construction which is the result of pro-unification of cxn with ts in direction,"
  (let* ((matching-pattern (matching-pattern cxn direction))
         (source (left-pole-structure ts))
         (couplings (pro-unify matching-pattern source))
         (new-cxn (copy-object cxn)))
    (setf (name new-cxn) (make-id (string-append "pro-unified-" (symbol-name  (name cxn)))))
    (setf (pole-structure (left-pole new-cxn)) (substitute-bindings couplings (left-pole-structure cxn)))
    (setf (pole-structure (right-pole new-cxn)) (substitute-bindings couplings (right-pole-structure cxn)))
    new-cxn))

(defun pro-unify (pattern source &optional (reduced-bindings nil) (renamings nil))
  "if different variables occuring in pattern are under
   matching with source consistently bound to the same value,
   they are replaced with multiple occurences of the same variable.
   returns nil if pattern and source don't unify.
   returns the renamings that were performed in the form of (var-to-be-renamed . var-to-be-renamed-in)
   e.g. (pro-unify '((?a (p ((n ?x) (q ?t))))
                     (?b (p ((m ?y) (f ?g)))))
                   '((unit-1 (p ((n l) (q p))))
                     (unit-2 (p ((m l) (f p))))))
        =>  ((FCG::?G . FCG::?T) (FCG::?Y . FCG::?X))"
  (let ((bindings (reverse (reverse-bindings (first (match-structures pattern source)))))) ;; is only first matching solution enough??
    (loop for (binding . var) in bindings
          do
          (if (assoc binding reduced-bindings :test 'equalp)
            ;; (variable-to-rename . renaming-variable) to renamings
            (push (cons var (cdr (assoc binding reduced-bindings :test 'equalp))) renamings)
            ;; otherwise, push (binding . var) to reduced bindings
            (push (cons binding var) reduced-bindings)))
    (or renamings
        +no-bindings+)))


