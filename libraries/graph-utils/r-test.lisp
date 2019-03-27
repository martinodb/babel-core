(ql:quickload :graph-utils)
(ql:quickload :uuid)
(ql:quickload :cl-store)

(use-package :graph-utils)

(defun make-cprefs (products)
  (let ((plen (length products)))
    (dolist (cid (select-flat (?x) (q- ?x :is-a :customer)))
      (dotimes (i 20)
        (add-triple cid :wants (nth (random plen) products)))
      (dotimes (i 100)
        (add-triple cid :ranks (nth (random plen) products) (1- (random 2.0)))))))

(defun make-r-graph (&key (user-count 1000) (product-count 10000))
  (let ((uuids (loop for x from 0 below (+ user-count product-count)
                    collecting (format nil "~A" (uuid:make-v1-uuid))))
        (graph (make-typed-graph :initial-edge-types
                                 '(:is-a :wants :ranks :sim)))
        (products nil) (customers nil))
    (init-prolog graph)
    (dotimes (i product-count)
      (let ((id (pop uuids)))
        (push id products)
        (add-triple id :is-a :product)))
    (dotimes (i (length products))
      (loop for j from (1+ i) below (length products) do
           (unless (= i j)
             (let ((p1 (nth i products))
                   (p2 (nth j products)))
               (let ((sim (1- (random 2.0))))
                 (add-triple p1 :sim p2 sim)
                 (add-triple p2 :sim p1 sim))))))
    (dotimes (i user-count)
      (let ((id (pop uuids)))
        (push id customers)
        (add-triple id :is-a :customer)))
    (make-cprefs products)
    (cl-store:store graph "rgraph.dat")
    graph))

(defparameter *graph* (make-r-graph))
