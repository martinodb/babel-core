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
(in-package :utils)

(export '(set-clos-slot get-clos-slot))

(defun set-clos-slot (instance slot value)
  "Set a clos-slot of an instance of a certain class to a given value"
  #+lispworks (setf (c2mop:slot-value-using-class (find-class (type-of instance))
                                     instance
                                     slot)
                    value)
  #+ccl (setf (slot-value instance slot) value))

(defun get-clos-slot (instance slot)
  "Get the clos-slot of an instance of a certain class"
  (when  (and (slot-exists-p instance slot)
              (slot-boundp instance slot))
    #+lispworks (c2mop:slot-value-using-class (find-class (type-of instance))
                                              instance
                                              slot)
    #+ccl (slot-value instance slot)))