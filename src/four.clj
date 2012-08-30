(ns four
  (import [javax.swing JFrame]
          [java.awt Color Canvas Dimension])
  (:gen-class))

(def w 800)
(def h 600)
(def hz 50)

(defn frame [g w h s]
  (let [c (Color. (rand-int 0xffffff))]
    (doto g
      (.setColor c)
      (.fillRect 0 0 w h)
      (.setColor Color/WHITE)
      (.drawString "4k.clj" 20 30))
    (assoc s :color c)))

(defn timeline [] (repeat frame))

(defn draw [c f s]
  (when (.isDisplayable c)
    (let [b (.getBufferStrategy c)
          g (.getDrawGraphics b)
          [w h] [(.getWidth c) (.getHeight c)]]
      (try
        (f g w h (update-in s [:frame] inc))
        (finally
         (.dispose g)
         (when-not (.contentsLost b)
           (.show b)))))))

(defn run [s f]
  (let [t (System/nanoTime)]
    (let [s (f s)
          d (- (System/nanoTime) t)
          l (/ 1000000000 hz)]
      (when (<  d l)
        (Thread/sleep (/ (- l d) 1000000)))
      (assoc s :duration d))))

(defn animate [d fs]
  (map #(partial d %) fs))

(defn canvas []
  (let [f (doto (JFrame.)
            (.setResizable false)
            .show
            (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
        c (Canvas.)]
    (doto (.getContentPane f)
      (.setPreferredSize (Dimension. w h))
      (.add c))
    (.pack f)
    (doto c
      (.setIgnoreRepaint true)
      (.createBufferStrategy 2))))

(defn start []
  (->> (timeline)
       (animate (partial draw (canvas)))
       (reductions run {:frame 0})
       (take-while identity)))

(defn -main []
  (dorun (start)))