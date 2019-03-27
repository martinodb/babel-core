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

(export '(draw-irl-program irl-program->svg))

(defun draw-irl-program (irl-program &key path (open nil) (format "png") draw-arrows)
  "Uses s-dot to draw an irl program. Returns the pathname of the
   generated graphic. When :open t, then it tries to open the
   irl-program. "
  (draw-predicate-network irl-program :path path :open open :format format :draw-arrows draw-arrows))

(defun irl-program->svg (irl-program &key draw-arrows (topic nil) (only-variables t))
  "renders irl-program into an svg"
  (predicate-network->svg irl-program :draw-arrows draw-arrows :topic topic :only-variables only-variables))



