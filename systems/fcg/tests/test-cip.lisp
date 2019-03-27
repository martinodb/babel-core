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

(deftest test-equivalent-meanings ()
  (test-assert (equivalent-meaning? '(girl x) '(girl x) :unify-with-instantiated-variables))
  (test-assert (not (equivalent-meaning? '(girl x) '(girl y) :unify-with-instantiated-variables)))
  (test-assert (equivalent-meaning? '(girl x) '(girl ?y) :unify-with-instantiated-variables))
  (test-assert (equivalent-meaning? '((unique ?x) (girl ?x)) '((unique ?y) (girl ?y)) :unify-with-instantiated-variables))
  (test-assert (equivalent-meaning? '((girl ?x) (unique ?x)) '((unique ?y) (girl ?y)) :unify-with-instantiated-variables))
  (test-assert (equivalent-meaning? '((unique ?x) (girl ?x)) '((unique ?a) (girl ?b)) :unify-with-instantiated-variables))
  (test-assert (equivalent-meaning? '((unique ?a) (girl ?b)) '((unique ?a) (girl ?b)) :unify-with-instantiated-variables))
  (test-assert (equivalent-meaning? '((unique ?a) (girl ?a)) '((girl ?a) (unique ?b)) :unify-with-instantiated-variables))
  (test-assert (equivalent-meaning? '((unique ?a) (girl ?a)) '((girl ?a) (unique ?b)) :unify-with-instantiated-variables))
  (test-assert (not (equivalent-meaning? '((unique ?a) (girl ?b)) '((girl ?a) (unique ?a)) :unify-with-instantiated-variables)))
  (test-assert (not (equivalent-meaning? '((unique ?b) (girl ?b)) '((girl ?a) (unique ?a) (dummy ?a)) :unify-with-instantiated-variables)))
  (test-assert (equivalent-meaning? '((unique ?b) (girl ?b)) '((girl ?a) (unique ?a)) :unify-with-instantiated-variables)))
  
;; (test-equivalent-meanings)