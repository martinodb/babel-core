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

(defun au-match-structures (pattern source bsl)
  "anti-unify structures in matching"
  (setf pattern (remove-J-units pattern))
  (when (<= (length pattern) (length source))
    (cond ((null pattern) bsl)
	  ((<= (length pattern) (length source))
	   (subset-p pattern source bsl :unify-fn #'(lambda (u1 u2 bsl)
						      (unify-units u1 u2 bsl))))
	  (t +fail+))))