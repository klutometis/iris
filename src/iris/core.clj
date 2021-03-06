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
                            StreamConfiguration))
  (:gen-class))

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

(def ^:dynamic *geobytes-email* nil)
(def ^:dynamic *geobytes-password* nil)

(def find-me
  (λ []
     (let [geolocation
           (:geobytes
            (read-json
             (:body
              (get "http://www.geobytes.com/IpLocator.htm"
                   {:query-params {"GetLocation" true
                                   "template" "json.txt"
                                   "pt_email" *geobytes-email*
                                   "pt_password" *geobytes-password*}}))))]
       {:latitude (:latitude geolocation)
        :longitude (:longitude geolocation)
        :city (:city geolocation)})))

(def locality-parser
  (λ [query]
     (let [parse-near-me
           (re-matches #"find (.+) near me" query)
           parse-in
           (re-matches #"find (.+) in (.+)" query)]
       (factual! *factual-key* *factual-secret*)
       (cond parse-near-me
             (let [[query what] parse-near-me
                   {latitude :latitude
                    longitude :longitude
                    city :city} (find-me)
                    results
                    (fetch :places
                           :q what
                           :geo {:$circle {:$center [latitude, longitude] :$meters 5000}}
                           :include_count true)]
               ;; We could just pass the city to the normal locality
               ;; parser.
               {:results results
                :quotable (format "\"%s\" near you" what)})
             parse-in
             (let [[query what where] parse-in]
               (let [results (fetch :places
                                    :q what
                                    ;; Let's `or'-this with address,
                                    ;; region, country.
                                    :filters {"locality" where}
                                    :include_count true)
                     quotable (format "\"%s\" in %s" what where)]
                 {:results results
                  :quotable quotable}))
             :else false))))

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
       (.start (Thread.
                (λ []
                   (read-line)
                   (.flush target)
                   (.stop target)
                   (.close target)
                   (println "I'm considering."))))
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

(def -main
  (λ [& args]
     (let [factual-key (clojure.core/get (System/getenv) "FACTUAL_KEY")
           factual-secret (clojure.core/get (System/getenv) "FACTUAL_SECRET")
           geobytes-email (clojure.core/get (System/getenv) "GEOBYTES_EMAIL")
           geobytes-password (clojure.core/get (System/getenv) "GEOBYTES_PASSWORD")]
       (binding [*factual-key* factual-key
                 *factual-secret* factual-secret
                 *geobytes-email* geobytes-email
                 *geobytes-password* geobytes-password]
         (loop []
           (read-line)
           (answer (consider (listen)))
           (recur))))))
