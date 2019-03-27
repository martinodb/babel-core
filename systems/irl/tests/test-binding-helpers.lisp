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

(defmacro test-merge-bindings (b1 b2 merged length)
  `(progn
     (multiple-value-bind (merged-res bindings-res)
         (merge-bindings ,b1 ,b2 :test #'equalp)
       (test-assert (eq ,merged merged-res)
                    "expected merged ~a, got ~a" ,merged merged-res)
       (test-assert (length= bindings-res ,length)
                    "expected result of length ~a, got ~a" ,length (length bindings-res)))
     (multiple-value-bind (merged-res bindings-res)
         (merge-bindings ,b2 ,b1 :test #'equalp)
       (test-assert (eq ,merged merged-res)
                    "(b2,b1) expected merged ~a, got ~a" ,merged merged-res)
       (test-assert (length= bindings-res ,length)
                    "(b2,b1) expected result of length ~a, got ~a" ,length (length bindings-res)))))

(deftest test-binding-helpers ()
  (test-merge-bindings nil
                       nil
                       t 0)
  (test-merge-bindings (make-bindings '((?x x)))
                       nil
                       t 1)
  (test-merge-bindings nil
                       (make-bindings '((?x x)))
                       t 1)
  (test-merge-bindings (make-bindings '((?x x)))
                       (make-bindings '((?y y)))
                       t 2)
  (test-merge-bindings (make-bindings '((?x x)))
                       (make-bindings '((?x x)))
                       t 1)
  (test-merge-bindings (make-bindings '((?x x)))
                       (make-bindings '((?x y)))
                       nil 0)
  (test-merge-bindings (make-bindings '((?x x)(?y y)(?z z)))
                       (make-bindings '((?z z)))
                       t 3)
  (test-merge-bindings (make-bindings '((?x x)(?y y)(?z z)))
                       (make-bindings '((?x x)(?y y)(?z z)))
                       t 3)
  (test-merge-bindings (make-bindings '((?x x)))
                       (make-bindings '((?x x)(?y y)(?z z)))
                       t 3))

;; (test-binding-helpers)
