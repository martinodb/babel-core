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

(defmacro testt-irl-program-connected? (irl-program connected number-of-classes)
  `(multiple-value-bind (ret-connected ret-number-of-classes)
       (irl-program-connected? ,irl-program)
     (test-assert (and (eq ,connected ret-connected)
                       (eq ,number-of-classes ret-number-of-classes))
                  "connected ~a, expected ~a and returned ~a classes, expected ~a"
                  ret-connected ,connected
                  ret-number-of-classes ,number-of-classes)))

(deftest test-irl-program-connected? ()
  (testt-irl-program-connected? '() t 0)
  (testt-irl-program-connected? '((bind t ?t t)) t 1)
  (testt-irl-program-connected? '((bind t ?t t)
                                  (bind t ?t1 t)) nil 2)
  (testt-irl-program-connected? '((bind t ?t t)
                                  (test ?t ?t1)
                                  (bind t ?t1 t)) t 1)
  (testt-irl-program-connected? '((bind t ?t t)
                                  (test ?t ?t1)
                                  (bind t ?t1 t)
                                  (test ?t1 ?t2)) t 1)
  (testt-irl-program-connected? '((bind t ?t t)
                                  (test ?t5 ?t3)
                                  (bind t ?t1 t)
                                  (test ?t1 ?t4)) nil 3))

;; (test-irl-program-connected?)
  