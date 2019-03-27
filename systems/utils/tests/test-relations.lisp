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

;; ############################################################################

(defclass ca (relatable-class) ((id :initarg :id :initform nil :accessor id)))
(defmethod copy-object-content ((source ca) (target ca))
  (setf (id target) (format nil "~a-copy" (id source))))

(defmethod print-object ((ca ca) stream)
  (format stream "<ca ~a>" (id ca)))

(defclass cb (relation-class) ((id :initarg :id :initform nil :accessor id)))
(defmethod copy-object-content ((source cb) (target cb))
  (setf (id target) (format nil "~a-copy" (id source))))

(defmethod print-object ((cb cb) stream)
  (format stream "<cb ~a>" (id cb)))

#-ecl ;; no copy-object-content in ecl
(deftest test-relation-copying ()
  (let* ((obj-1 (make-instance 'ca :id 'obj-1))
	 (obj-2 (make-instance 'ca :id 'obj-2))
	 (rel-1 (make-instance 'cb :id 'rel-1 :relatees `((role-1 . ,obj-1)
							  (role-2 . ,obj-2)))))
    (let* ((set (list obj-1 obj-2))
	   (set-copy (full-copy-object set))
	   (obj-1-copy (first set-copy))
	   (obj-2-copy (second set-copy)))

      (test-assert (equal (format nil "~a-copy" (id obj-1)) (id obj-1-copy)))
      (test-assert (equal (format nil "~a-copy" (id obj-2)) (id obj-2-copy)))

      (test-assert (= 1 (length (relaters obj-1))))
      (test-assert (eq 'role-1 (caar (relaters obj-1))))
      (test-assert (= 1 (length (cdar (relaters obj-1)))))
      (test-assert (eq rel-1 (cadar (relaters obj-1))))

      (test-assert (= 1 (length (relaters obj-2))))
      (test-assert (eq 'role-2 (caar (relaters obj-2))))
      (test-assert (= 1 (length (cdar (relaters obj-2)))))
      (test-assert (eq rel-1 (cadar (relaters obj-2))))

      (test-assert (= 1 (length (relaters obj-1-copy))))
      (test-assert (eq 'role-1 (caar (relaters obj-1-copy))))
      (test-assert (= 1 (length (cdar (relaters obj-1-copy)))))

      (test-assert (= 1 (length (relaters obj-2-copy))))
      (test-assert (eq 'role-2 (caar (relaters obj-2-copy))))
      (test-assert (= 1 (length (cdar (relaters obj-2-copy)))))

      (test-assert (eq (cadar (relaters obj-1-copy))
                       (cadar (relaters obj-2-copy))))
    
      (let ((rel-1-copy (cadar (relaters obj-1-copy))))
	(test-assert (equal (format nil "~a-copy" (id rel-1)) (id rel-1-copy)))

	(test-assert (= 2 (length (relatees rel-1))))
	(test-assert (loop for (role . relatee) in (relatees rel-1)
                           always
                           (or (and (eq 'role-1 role) (eq obj-1 relatee))
                               (and (eq 'role-2 role) (eq obj-2 relatee)))))
      
	(test-assert (= 2 (length (relatees rel-1-copy))))
	(test-assert (loop for (role . relatee) in (relatees rel-1-copy)
                           always
                           (or (and (eq 'role-1 role) (eq obj-1-copy relatee))
                               (and (eq 'role-2 role) (eq obj-2-copy relatee)))))))))


;; (test-relation-copying)

