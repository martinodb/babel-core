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

;;
;; This file contains functions that are normally NOT used anymore
;; and to which all references should be removed and adapted
;; (december 2016)
;; 
;;

;; This method is still used in a lot of tests, feel free to adapt the tests!!!

(defmethod de-render ((utterance list) (mode (eql :de-render-in-root-mode)) &key &allow-other-keys)
  "De-renders with a root-node and full form constraints."
  (warn "Deprecated Function: two-pole mode (as in FCG before 2015) is not supported anymore")
  (let ((strings nil)
        (sequence nil)
	(constraints nil))
    (dolist (string utterance)
      (let ((new (make-const string nil)))
        (push new sequence)
	(dolist (prev strings)
	  (push `(precedes ,(second prev) ,new) constraints))
	(push `(string ,new ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(meets ,(second (second left)) ,(second (first left)))
	    constraints))
    (make-instance 'coupled-feature-structure 
		   :left-pole '((root (meaning ()) (sem-cat ())))
		   :right-pole `((root (form ,(cons (cons 'sequence (reverse sequence))
                                                    (append strings constraints)))
                                       (syn-cat ()))))))

(defun make-default-cxn-set ()
  "Build a default construction-set."
  (warn "The function make-default-cxn-set is deprecated and should not be used anymore! Moreover, it is still tailored towards the root-mode from before 2015. Instead, just make an instance of a construction-set and set its configurations")
  (let ((cxn-set (make-instance 'construction-set )))
    ;;; Make sure that the root unit is used instead of the top:
    (set-configuration cxn-set :create-initial-structure-mode :root-mode) 
    (set-configuration cxn-set :de-render-mode :de-render-in-root-mode) 
    (set-configuration cxn-set :render-mode :render-in-root-mode) 
    ;;; Use construction sets:
    (set-configuration cxn-set :cxn-supplier-mode :ordered-by-label) 
    cxn-set))
