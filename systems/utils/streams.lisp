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

(export '(stream->list))

(defun stream->list (stream &key number-of-lines)
  "Returns a list of the next number-of-lines lines of the stream.
   If number-of-lines is unspecified, the entire stream is processed
   untill an empty line is encountered." 
  (let (lines)
    (do ((line (read-line stream nil nil)
               (read-line stream nil nil))
         (n 1 (1+ n)))
        ((if number-of-lines
           (or (> n number-of-lines) (null line))
           (null line))
         (reverse lines))
      (push line lines))))