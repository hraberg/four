(ns vlead
  (require [clojure.java.io :as io])
  (import [javax.sound.sampled AudioSystem AudioFormat AudioFileFormat$Type AudioInputStream SourceDataLine]
          [java.io ByteArrayInputStream]))

;; In remembrance of http://www.nada.kth.se/~raberg/vl.html, my 1996-97 Nord Lead simulator.
;; It more or less worked, was written in C++.

;; Lowpass filter and pulse width modulated by a triangle waves:
;; (play (chord [[:a 2 saw 0.7 (partial tri 4)]
;;               [:e 3 (partial sqr (partial tri 2)) 0.2]] 5))

;; The different components are chained together using partial and comp, no config DSL exists.
;; I don't like the way the time / current frame pollutes the oscillator generation. Early days.

(def bpm 120)
(def sample-rate 44100)
(def ^AudioFormat audio-format (AudioFormat. sample-rate 16 1 true true))

(def latency 10)
(def buffer-size (/ (* sample-rate (.getFrameSize audio-format)) (/ 1000 latency)))

(def buffer (byte-array buffer-size))
(def ^SourceDataLine line-out (AudioSystem/getSourceDataLine audio-format))

(defn frequency [offset-from-a4]
  (* 440.0 (Math/pow 2 (/ offset-from-a4 12.0))))

(def ^clojure.lang.PersistentVector notes [:a :a# :b :c :c# :d :d# :e :f :f# :g :g#])

(defn note [[n oct]]
  (frequency (+ (.indexOf notes n) (* 12 (- oct 4)))))

(defn period [^double hz]
  (/ sample-rate hz))

(defn as-period [^double x]
  (/ (+ 1 x) 2))

(defn amp [^double velocity ^double x]
  (* velocity x))

(defn phase [^double hz ^long t]
  (let [period (period hz)]
    (/ (mod t period) period)))

(defn phase-modulation [fm ^double hz ^long t]
  (* (as-period (fm t)) (phase hz t)))

(defn sin
  ([^double hz ^long t] (sin (constantly 1.0) hz t))
  ([fm ^double hz ^long t]
     (Math/sin (* (phase-modulation fm hz t) 2.0 Math/PI))))

(defn sqr
  ([^double hz ^long t] (sqr (constantly 0.5) hz t))
  ([pwm ^double hz ^long t]
     (if (< (phase hz t) (as-period (pwm t))) 1.0 -1.0)))

(defn tri
  ([^double hz ^long t] (tri (constantly 1.0) hz t))
  ([fm ^double hz ^long t]
     (let [phase (phase-modulation fm hz t)]
       (cond (< phase 0.25) (* phase 4)
             (< phase 0.75) (* (- phase 0.5) -4)
             :else (* (- phase 1) 4)))))

(defn saw
  ([^double hz ^long t] (saw (constantly 1.0) hz t))
  ([fm ^double hz ^long t]
     (let [phase (phase-modulation fm hz t)]
       (if (< phase 0.5) (* phase 2) (* (- phase 1) 2)))))

(defn rnd [_ _]
  (- (rand 2) 1))

(def seconds-per-beat (/ 60.0 bpm))
(defn note-length [length]
  (* length seconds-per-beat sample-rate))

(defn frequency-of-beat [beat]
  (/ sample-rate (note-length beat)))

(defn adsr [^double a ^double d ^double s ^double r]
  (let [[a d r] (map note-length [a d r])]
    [r
     (fn [^double l ^long t]
       (cond
        (>= t (- l r)) (- s (* (- t (- l r)) (/ s r)))
        (< t a) (/ t a)
        (< t (+ a d)) (- 1 (* (- t a) (/ (- 1 s) d)))
        :else s))]))

(defn envelope [vol length]
  (let [length (note-length length)]
    (condp some [vol]
      nil? [length (constantly 1)]
      number? [length (constantly vol)]
      vector? (let [[extra-length vol] vol
                    length (+ length extra-length)]
                [length (partial vol length)])
      [length (comp as-period vol)])))

;; Translated into Clojure from http://www.musicdsp.org/showArchiveComment.php?ArchiveID=26
(def poles 4)

(defn lp-filter [fc res samples]
  (let [in (double-array poles)
        out (double-array poles)]
    (->> samples
         (map-indexed (fn [^long t ^double input]
                        (let [f (* (fc t) 1.16)
                              fb (* (res t) (- 1.0 (* 0.15 f f)))]
                          (loop [input (-> input
                                           (- (* (aget out (dec poles)) fb))
                                           (* 0.35013 (* f f) (* f f)))
                                 idx 0]
                            (if (== idx poles)
                              input
                              (let [output (+ input
                                              (* 0.3 (aget in idx))
                                              (* (- 1 f) (aget out idx)))]
                                (aset out idx output)
                                (aset in idx input)
                                (recur output (inc idx)))))))))))

(defn velocity [vol samples]
  (->> samples
       (map-indexed (fn [^long t ^double input]
                      (* (vol t) input)))))

(defn oscillator [osc note]
  (->> (iterate inc 0)
       (map (partial osc note))))

(defn tone
  ([note length] (tone note length sin))
  ([[n oct & [note-osc vol fc res]] length osc]
     (let [[length vol] (envelope vol length)
           [_ fc] (envelope fc length)
           [_ res] (envelope res length)]
       (->> (oscillator (or note-osc osc) (note [n oct]))
            (velocity vol)
            (lp-filter (or fc (constantly 1)) (or res (constantly 0)))
            (take length)))))

(defn mix [& tracks]
  (/ (apply + tracks) (count tracks)))

(defn chord
  ([notes length] (chord notes length sin))
  ([notes length osc]
     (->> notes
          (map #(tone % length osc))
          (apply map mix))))

(defn clip [^double sample]
  (min 1.0 (max -1.0 sample)))

(def max-amplitude (dec (Math/pow 2 (dec (.getSampleSizeInBits audio-format)))))

(defn bits [^double sample]
  (Math/round (* sample max-amplitude)))

(defn write-sample-byte [^"[B" buffer ^long offset ^long sample]
  (let [high (unchecked-byte (+ (bit-and (bit-shift-right sample 8) 0xFF)))
        low (unchecked-byte (bit-and sample 0xFF))]
    (aset buffer offset high)
    (aset buffer (inc offset) low)
    buffer))

(defn write-sample-buffer [^"[B" buffer samples]
  (loop [i 0
         samples samples]
    (when samples
      (write-sample-byte buffer i (first samples))
      (recur (+ i 2) (next samples))))
  [buffer (* 2 (count samples))])

(defn out [[buffer available]]
  (.write line-out buffer 0 available))

(defn play [samples]
  (->> samples
       (map (comp bits clip))
       (partition (/ buffer-size 2) (/ buffer-size 2) (repeat 0))
       (map (comp out (partial write-sample-buffer buffer)))
       dorun))

(defn write
  ([samples] (write "test.wav" samples))
  ([file samples]
     (let [bytes (byte-array (* (count samples) (.getFrameSize audio-format)))]
       (write-sample-buffer bytes (map (comp bits clip) samples))
       (AudioSystem/write (AudioInputStream. (ByteArrayInputStream. bytes) audio-format (count samples))
                          AudioFileFormat$Type/WAVE (io/file file)))))

(defn start []
  (.open line-out audio-format buffer-size)
  (.start line-out))

(defn stop []
  (.stop line-out)
  (.close line-out))

(start)