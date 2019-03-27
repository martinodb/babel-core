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
(in-package :network)

(deftest test-complete-edges ()
  (let* ((node-1 (make-instance 'net-node))
	 (node-2 (make-instance 'net-node))
	 (edge (make-instance 'net-edge
				    :edge-label 'A->B
				    :edge-start node-1
				    :edge-end node-2))
	 (A+B-net (complete-edges
		   (make-instance 'network
				  :network-nodes (list node-1 node-2)
				  :network-edges (list edge)))))
    (test-assert (and (find edge (node-edges node-1))
		      (find edge (node-edges node-2))))
    (push (make-instance 'net-edge :edge-label 'bad) (node-edges node-1))
    (setf A+B-net (complete-edges A+B-net))
    (test-assert (and (= (length (node-edges node-1)) 1)
		      (find edge (node-edges node-1))))))

;; (test-complete-edges)

