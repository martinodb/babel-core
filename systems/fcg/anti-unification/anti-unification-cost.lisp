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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function for dispatching cost calculation to right function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-anti-unification-cost (cost-type cost-params pattern source)
  "if cost-type is found in cost-params, return cost of this cost-type
   otherwise return 0"
  (let ((cost (assoc cost-type cost-params :test 'string=)))
    (cond ((not cost)
           (warn "You didn't specify a cost for ~a, taking 0 as default cost." cost-type)
           0)
          ((numberp (second cost))
           (second cost))
          (t
           (let ((base-cost (funcall (second cost) pattern source)))
             (* base-cost (third cost)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for calculating cost based on pattern and source  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun depth-of-replaced-pattern (pattern source)
  (declare (ignore source))
  (if (variable-p pattern)
    0
    (1+ (depth pattern))))

