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

(deftest test-parent-configurations ()
  (let* ((parent-configuration
	  (make-configuration
	   :entries (list (cons 'key-a 'parent-value-a)
			  (cons 'key-b 'parent-value-b))))
	 (child-a-configuration
	  (make-configuration
	   :parent-configuration parent-configuration))
	 (child-b-configuration
	  (make-configuration
	   :parent-configuration parent-configuration)))
    (test-assert (eq (get-configuration parent-configuration 'key-a) 'parent-value-a))
    (test-assert (eq (get-configuration parent-configuration 'key-b) 'parent-value-b))
    (test-assert (null (get-configuration parent-configuration 'key-c)))
    ;; children inherit all defined values from parent
    (test-assert (eq (get-configuration child-a-configuration 'key-a) 'parent-value-a))
    (test-assert (eq (get-configuration child-a-configuration 'key-b) 'parent-value-b))
    (test-assert (null (get-configuration child-a-configuration 'key-c)))
    ;; setting a configuration at a lower level, only affects the child but not
    ;; its siblings, nor its parent
    (set-configuration child-a-configuration 'key-a 'child-value-a)
    (test-assert (eq (get-configuration parent-configuration 'key-a) 'parent-value-a))
    (test-assert (eq (get-configuration child-a-configuration 'key-a) 'child-value-a))
    (test-assert (eq (get-configuration child-b-configuration 'key-a) 'parent-value-a))
    ;; setting a configuration in a child does not add the configuration to its parent
    (set-configuration child-a-configuration 'key-c 'child-value-c)
    (test-assert (null (get-configuration parent-configuration 'key-c)))
    (test-assert (eq (get-configuration child-a-configuration 'key-c) 'child-value-c))
    (test-assert (null (get-configuration child-b-configuration 'key-c)))))
    
    
    