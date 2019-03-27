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


(deftest test-equivalent-irl-programs? ()
  (test-assert (equivalent-irl-programs? '((p1 ?a) (p2 ?a ?b) (p3 ?b))
                                         '((p1 ?x) (p2 ?x ?y) (p3 ?y))))

  (test-assert (equivalent-irl-programs? '((p1 ?a) (p2 ?a ?b) (p3 ?b))
                                         '((p1 ?a) (p2 ?a ?b) (p3 ?b))))

  (test-assert (equivalent-irl-programs? '((p1 ?a) (p2 ?a ?b) (p3 ?b))
                                         '((p1 ?b) (p2 ?b ?a) (p3 ?a))))
  
  (test-assert (not (equivalent-irl-programs? '((p1 ?a) (p2 ?a ?b) (p3 ?b))
                                              '((p1 ?a) (p2 ?b ?a) (p3 ?b)))))
  
  (test-assert (equivalent-irl-programs? '((p1 ?a) (p2 ?b))
                                         '((p1 ?x) (p2 ?y))))
  
  (test-assert (not (equivalent-irl-programs? '((p1 ?a) (p2 ?b))
                                              '((p1 ?x) (p2 ?x)))))
  
  (test-assert (not (equivalent-irl-programs?  '((foo ?a ?b) (bar ?a)) 
                                               '((foo ?a ?b) (bar ?b)))))

  (test-assert (not (equivalent-irl-programs?
                     '((prim-1 ?bar ?foo) (prim-1 ?foo ?baz) (prim-2 ?baz))
                     '((prim-1 ?bar ?foo) (prim-1 ?foo ?baz) (prim-2 ?foo)))))
  
  (test-assert (not (equivalent-irl-programs?
                     '((a ?a) (p ?a ?b) (b ?b) (c ?c) (p ?c ?d) (d ?d))
                     '((a ?a) (p ?a ?d) (d ?d) (c ?c) (p ?c ?b) (b ?b))))))


;;(test-equivalent-irl-programs?)