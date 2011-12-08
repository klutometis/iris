(ns iris.core
  (:use clojure.data.json
        lambda.core
        funnyplaces.api
        debug.core
        clj-http.client
        clojure.java.io
        cadr.core)
  (:import (java.util Random
                      Timer
                      TimerTask)
           (java.io File
                    FileOutputStream)
           (javax.sound.sampled AudioFormat
                                AudioSystem
                                AudioInputStream
                                AudioFileFormat
                                AudioFileFormat$Type)
           (javazoom.jl.player Player)
           (javaFlacEncoder FLAC_FileEncoder
                            StreamConfiguration)))

(def ^:dynamic *factual-key* nil)

(def ^:dynamic *factual-secret* nil)

(let [random (new Random (System/currentTimeMillis))]
  (def random-element
    (λ [list]
       (nth list (.nextInt random (count list))))))

(def ^:dynamic *prefix* "iris")
         
(def create-temporary-file
  (λ [suffix] (File/createTempFile *prefix* suffix)))
    
(def create-temporary-mp3
  (λ [] (create-temporary-file ".mp3")))

(def create-temporary-wave
  (λ [] (create-temporary-file ".wav")))

(def create-temporary-flac
  (λ [] (create-temporary-file ".flac")))

(def default-parser
  (λ [query]
     (factual! *factual-key* *factual-secret*)
     (let [results (fetch :places
                          :q query
                          :include_count true)
           quotable (format "\"%s\"" query)]
       {:results results
        :quotable quotable})))

(def locality-parser
  (λ [query]
     (let [[query what where]
           (re-matches #"find (.+) in (.+)" query)]
       (if (and what where)
         (do (factual! *factual-key* *factual-secret*)
             (let [results (fetch :places
                                  :q what
                                  :filters {"locality" where}
                                  :include_count true)
                   quotable (format "\"%s\" in %s" what where)]
               {:results results
                :quotable quotable}))
         false))))

(def parsers (list locality-parser
                   default-parser))

(def parse-query
  (λ [query]
     (loop [parsers parsers]
       (if (empty? parsers)
         {:results []
          :quotable (format "\"%s\"" query)}
         (let [parser (car parsers)
               result (parser query)]
           (or result (recur (cdr parsers))))))))

(def consider
  (λ [query]
     (println (format "I understood, \"%s.\"" query))
     (let [{results :results
            quotable :quotable}
           (parse-query query),
           {total :total_row_count
            included :included_rows}
           (:response (meta results))]
       (cond (empty? results)
             (format "I couldn't find any places for %s." quotable)
             (= 1 total)
             (format "The only place for %s appears to be %s."
                     quotable
                     (:name (car results)))
             :else
             (format "Of the %s or so places for %s, you might like %s."
                     total
                     quotable
                     (:name (random-element results)))))))

(def answer
  (λ [response]
     (println response)
     (let [mp3 (:body (get "http://translate.google.com/translate_tts"
                           {:query-params {"ie" "UTF-8"
                                           "tl" "en"
                                           "q" response
                                           }
                            :as :byte-array}))
           file (create-temporary-mp3)]
       (with-open [file (FileOutputStream. file)]
         (.write file mp3))
       (with-open [player (new Player (input-stream file))]
         (.play player)))))

(def ^:dynamic *input-index* 
  "Default index of the recording device; NB: this is a hack."
  1)

(def ^:dynamic *sample-rate* 8000)

(def ^:dynamic *sample-size* 16)

(def ^:dynamic *channels* 1)

(def ^:dynamic *signed* true)

(def ^:dynamic *big-endian* false)

(def ^:dynamic *format*
  (new AudioFormat
       *sample-rate*
       *sample-size*
       *channels*
       *signed*
       *big-endian*))

(def sort-hypotheses
  (λ [hypotheses]
     (sort-by (λ [hypothesis]
                 (let [{utterance :utterance confidence :confidence}
                       hypothesis]
                   confidence))
              >
              hypotheses)))

(def parse-response
  (λ [response]
     (let [{status :status
            id :id
            hypotheses :hypotheses}
           (read-json response)
           {utterance :utterance
            confidence :confidence}
           (car (sort-hypotheses hypotheses))]
       utterance)))

(def ^:dynamic *google-url*
  "https://www.google.com/speech-api/v1/recognize?xjerr=1&client=chromium&lang=en-US")

(def post-to-google
  (λ [flac]
     (:body
      (clj-http.client/post
       *google-url*
       {:multipart [["Content" flac]]
        :headers {"Content-type"
                  (format "audio/x-flac; rate=%s" *sample-rate*)}}))))

(def listen
  (λ []
     (let [mixer-info (clojure.core/get (AudioSystem/getMixerInfo) *input-index*)
           target (AudioSystem/getTargetDataLine *format* mixer-info)]
       ;; `with-open'?
       (.open target *format*)
       (println "I'm listening.")
       (.start target)
       (let [timer (new Timer)
             task (proxy [TimerTask] []
                    (run []
                      (.flush target)
                      (.stop target)
                      (.close target)
                      (println "I'm considering.")
                      (.cancel timer)))]
         (.schedule timer task 10000))
       (let [input-stream (new AudioInputStream target)]
         (let [wave (create-temporary-wave)
               flac (create-temporary-flac)]
           (AudioSystem/write input-stream
                              AudioFileFormat$Type/WAVE
                              wave)
           (let [encoder (new FLAC_FileEncoder)]
             (.setStreamConfig encoder
                               (new StreamConfiguration
                                    *channels*
                                    StreamConfiguration/DEFAULT_MIN_BLOCK_SIZE
                                    StreamConfiguration/DEFAULT_MAX_BLOCK_SIZE
                                    *sample-rate*
                                    *sample-size*))
             (.encode encoder wave flac)
             (parse-response (post-to-google flac))))))))
