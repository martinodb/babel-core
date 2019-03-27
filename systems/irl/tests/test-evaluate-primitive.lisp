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
(in-package :irl)

;;;;;;; some tests for evaluate-primitive
(deftest test-evaluate-primitive ()
         ;; give zero arguments
         (test-assert (null (evaluate-primitive 'pick-apples `(,(make-instance 'binding
                                                                          :var '?var-1
                                                                          :score 0.5
                                                                          :value nil)
                                                          ,(make-instance 'binding
                                                                          :var '?var-2
                                                                          :score 0.5
                                                                          :value nil))
                                                *test-ontology*)))
         ;; give first
         (test-assert (= 1 (length (evaluate-primitive
                                    'pick-apples
                                    (list (make-instance 'binding
                                                         :var '?var-1
                                                         :score nil
                                                         :value nil)
                                          (make-instance
                                           'binding
                                           :var '?var-2
                                           :score 0.5
                                           :value (make-instance 'quantity :n 12)))
                                    *test-ontology*))))
         ;; give second
         (test-assert (= 1 (length (evaluate-primitive
                                    'pick-apples
                                    (list (make-instance 'binding
                                                         :var '?var-1
                                                         :score 0.5
                                                         :value (make-apples-set 20))
                                          (make-instance 'binding
                                                         :var '?var-2
                                                         :score nil
                                                         :value nil))
                                    *test-ontology*))))
         ;; give both consistent
         (test-assert (= 1 (length (evaluate-primitive
                                    'pick-apples
                                    (list (make-instance 'binding
                                                         :var '?var-1
                                                         :score 0.5
                                                         :value (make-apples-set 20))
                                          (make-instance
                                           'binding
                                           :var '?var-2
                                           :score 0.5
                                           :value (make-instance 'quantity :n 20)))
                                    *test-ontology*))))
         ;; give both inconsistent
         (test-equal 'inconsistent (evaluate-primitive
                                    'pick-apples
                                    (list (make-instance 'binding
                                                         :var '?var-1
                                                         :score 0.5
                                                         :value (make-apples-set 20))
                                          (make-instance
                                           'binding
                                           :var '?var-2
                                           :score 0.5
                                           :value (make-instance 'quantity :n 12)))
                                    *test-ontology*)))

;;(test-evaluate-primitive)