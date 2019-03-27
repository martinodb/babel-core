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

;; ############################################################################
;; Initial structures for root-mode
;; ############################################################################

(defmethod create-initial-structure ((meaning list) (mode t))
  (make-instance 'coupled-feature-structure
                 :left-pole `((root
			       (referent)
			       (meaning ,meaning)
			       (sem-cat nil)))
		 :right-pole '((root
				(form nil)
				(syn-cat nil)))
		 :left-pole-domain 'sem
		 :right-pole-domain 'syn))

(defmethod create-initial-structure ((meaning list)
                                     (mode (eql :root-mode)))
  (create-initial-structure meaning t))

(defmethod create-initial-structure ((meaning list)
                                     (mode (eql :root-mode-replace-variables-with-symbols)))
  (create-initial-structure (replace-variables-with-symbols meaning) t))

;; ############################################################################
;; Initial structures for one-pole-mode
;; ############################################################################

(defmethod create-initial-structure ((meaning list)
                                     (mode (eql :one-pole-mode)))
  (make-instance 'coupled-feature-structure
                 :left-pole `((root
			       (referent)
			       (meaning ,meaning)
			       (sem-cat nil)
                               (form nil)
                               (syn-cat nil)))
		 :right-pole '((root))
		 :left-pole-domain 'sem
		 :right-pole-domain 'syn))

