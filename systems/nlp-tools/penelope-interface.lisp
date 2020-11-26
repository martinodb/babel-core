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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interfacing with the NLP tools provided by the Penelope web service  ;;
;; Katrien and Paul, October 2017                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :nlp-tools)

(export '(get-penelope-lemmas
          get-penelope-noun-chunks
          get-penelope-named-entities
          get-penelope-pos-tags
          run-penelope-dependency-parser
          get-penelope-dependency-analysis
          get-penelope-dependency-analyses
          get-penelope-tokens
          get-penelope-sentence-tokens
          get-penelope-text-tokens
          get-word-similarity
          get-penelope-word-embeddings
          get-penelope-syntactic-analysis
          curl-json
          guardian-data
          glove))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running spacy services locally              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;By default, the NLP tools use the Spacy API that is running on the VUB AI Lab server:
 (defparameter *penelope-host* "https://penelope.vub.be/spacy-api")

;;You can run the services also locally, if you clone the spacy-api repository (gitlab ehai) and follow the readme file there. Once your python server is running, please evaluate this line:
;;(defparameter *penelope-host* "http://localhost:5000")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interfacing with using http request and json ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+lispworks
(defun send-request (route json &key (host *penelope-host*))
  "Send curl request and returns the answer."
  (let* ((url (string-append host route))
         (response (drakma:http-request url
                                        :method :post
                                        :content-type "application/json"
                                        :content json)))
    (when response (handler-case (cl-json:decode-json-from-string response)
                     (error (e)
                       (format t "Error in response from spacy API service [nlp-tools penelope-interface]: ~S.~&" e))))))
#-lispworks
(defun send-request (route json &key (host *penelope-host*))
  "Send curl request and returns the answer."
  (let* ((url (string-append host route))
         (response (dex:post url
                             :headers '((Content-Type . "application/json"))
                             :content json)))
    (when response (handler-case (cl-json:decode-json-from-string response)
                     (error (e)
                       (format t "Error in response from spacy API service [nlp-tools penelope-interface]: ~S.~&" e))))))


;;;;;;;;;;;;;;;;;;;;;;;
;; Sentence as input ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Tokenization ;;
;;;;;;;;;;;;;;;;;;

(defun run-penelope-tokenizer (sentence &key (model "en"))
  "Call the penelope server to tokenize a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-tokenizer> expects a string as input"))
  (send-request "/tokenize"
             (encode-json-to-string `((:sentence . ,sentence)
                                                (:model . ,model)))))

;;(run-penelope-tokenizer "Paul kicked the ball.")

(defun get-penelope-tokens (sentence &key (model "en"))
  "Returns tokens."
  (rest (assoc :tokens (run-penelope-tokenizer sentence :model model))))

;; (get-penelope-tokens "Paul kicked the ball.")


;; Lemmatization ;;
;;;;;;;;;;;;;;;;;;;

(defun run-penelope-lemmatizer (sentence &key (model "en"))
  "Call the penelope server to get the lemmatize the words in the input sentence."
  (unless (stringp sentence)
    (if (listp sentence)
      (setf sentence (format nil "~{~a~^ ~}" sentence))
      (error "The function <run-penelope-lemmatizer> expects a string as input")))
  (send-request "/lemmatize" (encode-json-to-string `((:sentence . ,sentence) (:model . ,model)))))

;; (run-penelope-lemmatizer "April is the fourth month of the year")

(defun get-penelope-lemmas (sentence &key (model "en"))
  "Takes a sentence as a string as input and returns a list with lists
of strings, each list corresponding to a noun chunk."
    (rest (assoc :lemmas (run-penelope-lemmatizer sentence :model model))))

;; (get-penelope-lemmas "April is the fourth month of the year.")


;; Noun chunks     ;;
;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-noun-chunker (sentence &key (model "en"))
  "Call the penelope server to get the noun chunks in a sentence."
  (unless (stringp sentence)
    (if (listp sentence)
      (setf sentence (format nil "~{~a~^ ~}" sentence))
      (error "The function <run-penelope-noun-chunker> expects a string as input")))
          (send-request "/noun-chunks"
                     (encode-json-to-string `((:sentence . ,sentence)
                                                        (:model . ,model)))))

;; (run-penelope-noun-chunker "April is the fourth month of the year")

(defun get-penelope-noun-chunks (sentence)
  "Takes a sentence as a string as input and returns a list with lists
of strings, each list corresponding to a noun chunk."
  (rest (assoc :noun--chunks (run-penelope-noun-chunker sentence))))

;; (get-penelope-noun-chunks "April is the fourth month of the year.")


;; POS tags ;;
;;;;;;;;;;;;;;

(defun run-penelope-pos-tagger (sentence &key (model "en"))
  "Call the penelope server to get the POS tags for a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-pos-tagger> expects a string as input"))
         (send-request "/pos"
                    (encode-json-to-string `((:sentence . ,(remove-multiple-spaces sentence))
                                                       (:model . ,model)))))

;; (run-penelope-pos-tagger "April is the fourt month of the year.")

(defun get-penelope-pos-tags (transient-structure/sentence)
  "Takes a sentence as a string as input and returns a list with lists
of strings, each list corresponding to a word with its most likely POS tag."
  (let* ((sentence (if (stringp transient-structure/sentence)
                     transient-structure/sentence
                     (if (stringp (get-data transient-structure/sentence :utterance))
                       (get-data transient-structure/sentence :utterance)
                       (list-of-strings->string (get-data transient-structure/sentence :utterance)))))
         (penelope-pos-tags (rest (assoc :tagged--words (run-penelope-pos-tagger sentence)))))
    (loop for entry in penelope-pos-tags
          for word = (rest (assoc :text entry ))
          for tag = (rest (assoc :tag entry ))
          collect (list word tag))))

;; (get-penelope-pos-tags "April is the fourth month of the year.")


;;Named Entity Recognition ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-named-entity-recognition (sentence &key (model "en"))
  "Call the penelope server to get the named entities from a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-named-entity-recognition> expects a string as input"))
  (send-request "/named-entities"
             (encode-json-to-string `((:sentence . ,sentence)
                                                (:model . ,model)))))

;; (run-penelope-named-entity-recognition "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date.")

;;(run-penelope-named-entity-recognition "serbia is a country")

(defun get-penelope-named-entities (transient-structure/sentence)
  "Takes a sentence as a string as input and returns a list with lists
of strings, each list corresponding to a named entity."
  (let* ((sentence (if (stringp transient-structure/sentence)
                     transient-structure/sentence
                     (if (stringp (get-data transient-structure/sentence :utterance))
                       (get-data transient-structure/sentence :utterance)
                       (list-of-strings->string (get-data transient-structure/sentence :utterance)))))
         (penelope-named-entities (rest (assoc :named--entities (run-penelope-named-entity-recognition sentence)))))
    (loop for entry in penelope-named-entities
          for entity = (rest (assoc :text entry ))
          for type = (rest (assoc :ent entry ))
          collect (list entity type))))

;; (get-penelope-named-entities "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date.")


;;Dependency parsing ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-dependency-parser (sentence &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-dependency-parser> expects a string as input"))
  (send-request "/dependencies"
             (encode-json-to-string `((:sentence . ,(remove-multiple-spaces sentence))
                                                (:model . ,model)))))

;; (run-penelope-dependency-parser "April is the fourth month of the year")

(defun get-penelope-dependency-analysis (utterance &key (model "en"))
  "Returns a dependency tree analysis."
  (rest (assoc :tree (first (rest (assoc :dependencies (run-penelope-dependency-parser utterance :model model)))))))
;;(get-penelope-dependency-analysis "April is the fourth month of the year")

(defun get-penelope-dependency-analyses (utterances &key (model "en"))
  "Returns dependency tree analyses of text consisting of multiple sentences."
  (rest (assoc :dependencies (run-penelope-dependency-parser utterances :model model))))

;; Constituency and Dependency parsing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-syntactic-parser (sentence &key (model "en_benepar"))
  "Call the penelope server to get the dependency and constituency structure of a sentence."
  (unless (or (stringp sentence)
              (listp sentence))
    (error "The function <run-penelope-dependency-parser> expects a string or a list as input"))
  (send-request "/syntactic-parser"
             (encode-json-to-string `((:sentence . ,(if (stringp sentence)
                                                      (remove-multiple-spaces sentence)
                                                      sentence))
                                      (:model . ,model)))))

;; (run-penelope-syntactic-parser "April is the fourth month of the year")

(defun get-penelope-syntactic-analysis (utterance &key (model "en_benepar"))
  "Returns a syntacic tree analysis in the form of constituents and dependents."
   (rest (assoc :tree (first (rest (assoc :trees (run-penelope-syntactic-parser utterance :model model)))))))

;;(get-penelope-syntactic-analysis "April is the fourth month of the year")
;;(get-penelope-syntactic-analysis '("April" "is" "the" "fourth" "month" "of" "the" "year"))

;; Word embeddings ;;
;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-sentence-word-embeddings (sentence &key (model "en"))
  (warn "Deprecated function, call run-penelope-word-embeddings instead.")
  (run-penelope-word-embeddings sentence :model model))

(defun run-penelope-word-embeddings (sentence &key (model "en"))
  "Call the penelope server to get the word embeddings of a single sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-sentence-word-embeddings> expects a string as input"))
  (send-request "/embeddings"
                (encode-json-to-string `((:sentence . ,sentence)
                                         (:model . ,model)))))

;; (run-penelope-word-embeddings "ball boy ball")

(defun get-penelope-word-embeddings (sentence &key (lemmatize? nil))
  "Get the word embeddings for a sentence in a '((word1 vector1) (word2 vector2)) format."
  (when lemmatize?
    (setf sentence (list-of-strings->string (get-penelope-lemmas sentence))))
  (let ((penelope-embeddings (rest (assoc :vectors (run-penelope-word-embeddings sentence)))))
    (loop for word-embedding in penelope-embeddings
          for token = (rest (assoc :token word-embedding))
          for vector = (rest (assoc :vector word-embedding))
          collect (list token vector))))

;; (get-penelope-word-embeddings "ball boy ball")


;;;;;;;;;;;;;;;;;;;;;;;
;; Text as input     ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Sentence Tokenizer ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-sentence-tokenizer (sentences &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (stringp sentences)
    (error "The function <run-penelope-sentence-tokenizer> expects a string as input"))
         (send-request "/split-sentences"
                    (encode-json-to-string `((:text . ,sentences) (:model . ,model)))))

;; (run-penelope-sentence-tokenizer "Paul kicked the ball. Mary caught it.")

(defun get-penelope-sentence-tokens (sentences &key (model "en"))
  "Returns sentences."
  (rest (assoc :sentences (run-penelope-sentence-tokenizer sentences :model model))))

;; (get-penelope-sentence-tokens "Paul kicked the ball. Mary caught it.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List of texts as input ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These function take lists of texts as input, split them into sentences and perform the requested
;; analysis on each sentence

;; Tokenization ;;
;;;;;;;;;;;;;;;;;;

(defun run-penelope-texts-tokenizer (texts &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (listp texts)
    (error "The function <run-penelope-texts-tokenizer> expects a list as input"))
         (send-request "/texts-tokenize"
                    (encode-json-to-string `((:texts . ,texts)
                                                       (:model . ,model)))))

;; (run-penelope-texts-tokenizer '("This is one article. And it has two sentences" "Then there is a second article. It talks about Mr. Smith."))


(defun get-penelope-texts-tokens (texts &key (model "en"))
       (rest (assoc :texts--tokens
                    (run-penelope-texts-tokenizer texts :model model))))

;; (get-penelope-texts-tokens '("This is one article. And it has two sentences" "Then there is a second article. It talks about Mr. Smith."))


;; Lemmatization ;;
;;;;;;;;;;;;;;;;;;;

(defun run-penelope-texts-lemmatizer (texts &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (listp texts)
    (error "The function <run-penelope-texts-lemmatizer> expects a list as input"))
         (send-request "/texts-lemmatize"
                    (encode-json-to-string `((:texts . ,texts)
                                                       (:model . ,model)))))

;; (run-penelope-texts-lemmatizer '("This is one article. And it has two sentences" "Then there is a second article. It talks about Mr. Smith."))


(defun get-penelope-texts-lemmas (texts &key (model "en"))
       (rest (assoc :texts--lemmas
                    (run-penelope-texts-lemmatizer texts :model model))))

;; (get-penelope-texts-lemmas '( "Then there is a second article. It talks about Mr. Smith."))


;; Noun chunks     ;;
;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-texts-noun-chunker (texts &key (model "en"))
  (unless (listp texts)
    (error "The function <run-penelope-texts-noun-chunker> expects a list as input"))
         (send-request "/texts-noun-chunks"
                    (encode-json-to-string `((:texts . ,texts)
                                                       (:model . ,model)))))

;; (run-penelope-texts-noun-chunker '("April is the fourth month of the year. May is the fifth month" "My name is Obama"))

(defun get-penelope-texts-noun-chunks (texts &key (model "en"))
       (rest (assoc :texts--noun--chunks
                    (run-penelope-texts-noun-chunker texts :model model))))

;; (get-penelope-texts-noun-chunks '("April is the fourth month of the year. May is the fith month" "this is another sentence"))


;; POS tags ;;
;;;;;;;;;;;;;;

(defun run-penelope-texts-pos-tagger (texts &key (model "en"))
  (unless (listp texts)
    (error "The function <run-penelope-texts-noun-chunker> expects a list as input"))
         (send-request "/texts-pos-tags"
                    (encode-json-to-string `((:texts . ,texts)
                                                       (:model . ,model)))))

;; (run-penelope-texts-pos-tagger '("April is the fourth month of the year. May is the fith month" "this is another sentence with Obama"))

(defun get-penelope-texts-pos-tags (texts &key (model "en"))
       (rest (assoc :texts--pos--tags
                    (run-penelope-texts-pos-tagger texts :model model))))

;; (get-penelope-texts-pos-tags '("April is the fourth month of the year. May is the fith month" "this is another sentence"))


;;Named Entity Recognition ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-texts-named-entity-recognition (texts &key (model "en"))
  (unless (listp texts)
    (error "The function <run-penelope-texts-noun-chunker> expects a list as input"))
         (send-request "/texts-named-entities"
                    (encode-json-to-string `((:texts . ,texts)
                                                       (:model . ,model)))))

;; (run-penelope-texts-named-entity-recognition '("April is the fourth month of the year. May is the fith month" "this is another sentence with Obama"))

(defun get-penelope-texts-named-entities (texts &key (model "en"))
       (rest (assoc :texts--named--entities
                    (run-penelope-texts-named-entity-recognition texts :model model))))

;; (get-penelope-texts-named-entities '("April is the fourth month of the year. May is the fifth month" "I like Obama"))
;; (get-penelope-texts-named-entities '("We zullen morgen naar Parijs gaan. En daarna naar huis." "Hij werkt voor Artsen Zonder Grenzen") :model "nl")


;;Dependency parsing ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-texts-dependency-parser (sentence &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (listp sentence)
    (error "The function <run-penelope-texts-dependency-parser> expects a list as input"))
  (send-request "/texts-dependencies"
             (encode-json-to-string `((:texts . ,sentence)
                                                (:model . ,model)))))

;;(run-penelope-texts-dependency-parser '("April is the fourth month of the year. May is the fifth month" "I am Paul"))

(defun get-penelope-texts-dependency-analysis (utterance &key (model "en"))
  "Returns a dependency tree analysis."
(mapcar #'(lambda (document) (mapcar #'(lambda (sentence) (rest (assoc :tree sentence))) (first document)))
        (rest (assoc :texts--dependencies (run-penelope-texts-dependency-parser utterance :model model)))))

;;(get-penelope-texts-dependency-analysis '("April is the fourth month of the year. May is the fifth month" "I am Paul"))

;; Word embeddings ;;
;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-texts-word-embeddings (texts &key (model "en"))
  (unless (listp texts)
    (error "The function <run-penelope-texts-word-embeddings> expects a list as input"))
         (send-request "/texts-embeddings"
                    (encode-json-to-string `((:texts . ,texts)
                                                       (:model . ,model)))))

;; (run-penelope-texts-word-embeddings '("fish. I like fish." "I like meat"))

(defun get-penelope-texts-word-embeddings (texts &key (model "en"))
       (rest (assoc :texts--vectors
                    (run-penelope-texts-word-embeddings texts :model model))))

;; (get-penelope-texts-word-embeddings '("fish. I like fish." "I like meat"))

;; Sentence tokenization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-texts-sentence-tokenizer (texts &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (listp texts)
    (error "The function <run-penelope-texts-sentence-tokenizer> expects a list of strings as input"))
         (send-request "/texts-split-sentences"
                    (encode-json-to-string `((:texts . ,texts) (:model . ,model)))))

;; (run-penelope-texts-sentence-tokenizer '("This is one article. And it has sentences" "Then there is a second article. It talks about Mr. Smith."))


(defun get-penelope-texts-sentence-tokens (texts &key (model "en"))
       (rest (assoc :texts--sentences
                    (run-penelope-texts-sentence-tokenizer texts :model model))))

;; (get-penelope-texts-sentence-tokens '("This is one article. And it has two sentences" "Then there is a second article. It talks about Mr. Smith."))



(defun get-word-embeddings (sentence &key (lemmatize? nil)) 
  "Get the word embeddings for a sentence in a '((word1 vector1) (word2 vector2)) format."
  (when lemmatize?
    (setf sentence (list-of-strings->string (get-penelope-lemmas sentence))))
  (let ((penelope-embeddings (run-penelope-word-embeddings sentence)))
    (loop for word-embedding in (rest (assoc :vectors penelope-embeddings))
          for token = (rest (assoc :token word-embedding))
          for vector = (rest (assoc :vector word-embedding))
          collect (list token vector))))

; (get-word-embeddings "hello world")
; (cosine-similarity (second (first (get-word-embeddings "girl"))) (second (first (get-word-embeddings "girls" :lemmatize? t))))


(defun get-word-similarity (word1 word2 &key (lemmatize? nil)) 
  "Calculates the cosine similarity between two words based on the word embeddings from Glove."
  (let ((vector1 (second (first (get-word-embeddings word1  :lemmatize? lemmatize?))))
        (vector2 (second (first (get-word-embeddings word2  :lemmatize? lemmatize?)))))
    (cosine-similarity vector1 vector2)))

;;(get-word-similarity "boy" "banana")
;;(get-word-similarity "banana" "banana")


(defun get-phrase-similarity (phrase1 phrase2 &key (lemmatize? nil))
  ;;multiply? pretty girl vs handsome boy
  (let* ((vectors-for-phrase-1
          (mapcar #'(lambda (word)
                      (second (first (get-word-embeddings word :lemmatize? lemmatize?))))
                  (split-sequence:split-sequence #\Space phrase1)))
         (vector1 (utils::multiply-list-of-vectors vectors-for-phrase-1))
         (vectors-for-phrase-2
          (mapcar #'(lambda (word)
                      (second (first (get-word-embeddings word :lemmatize? lemmatize?))))
                  (split-sequence:split-sequence #\Space phrase2)))
         (vector2 (utils::multiply-list-of-vectors vectors-for-phrase-2)))
    (utils::cosine-similarity vector1 vector2)))

;;(get-phrase-similarity "Mickey Mouse" "Trump"  :lemmatize? nil)
;;(get-phrase-similarity "handsome boy" "pretty girl" )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preprocessing sentences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(sentence->bag-of-words))

(defun sentence->bag-of-words (sentence &key lemmatize? (language "en"))
  "Given a sentence as a string, this function turns it into a list of
words (strings), optionally lemmatized. It does not conserve
punctuation marks."
    ;;1) remove punctuation
    (setf sentence
          (coerce (loop for char across sentence
                        unless (member char '(#\, #\. #\" #\; #\- #\Tab #\Newline ;#\•
                                                  ))
                        collect char) 'string))

    ;;2) lemmatize if needed, otherwise just tokenize
    (if lemmatize?
      (setf sentence
            (get-penelope-lemmas sentence :model language))
      (setf sentence
            (get-penelope-tokens sentence :model language)))

    sentence)

;; (sentence->bag-of-words "I will go to the gym tomorrow." :lemmatize? t)






