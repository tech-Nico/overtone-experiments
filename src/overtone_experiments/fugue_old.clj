(ns overtone-experiments.fugue
  (:use [overtone.live]))

(definst organ1
  [note 60 a 0.3 d 2 s 3 r 1 g 1]
  (let [freq  (midicps note)
        waves (sin-osc [(* 0.5 freq)
                        freq
                        (* (/ 3 2) freq)
                        (* 2 freq)
                        (* freq 2 (/ 3 2))
                        (* freq 2 2)
                        (* freq 2 2 (/ 5 4))
                        (* freq 2 2 (/ 3 2))
                        (* freq 2 2 2)])
        snd   (apply + waves)
        env   (env-gen (adsr a d s r) g 1 0 1 FREE)]
    (* env snd 0.4)))

(definst organ2
  [note 60 a 0.1 d 1 s 1 r 0.3 g 2]
  (let [freq  (midicps note)
        waves (sin-osc [(* 0.5 freq)
                        freq
                        (* (/ 3 2) freq)
                        (* 2 freq)
                        (* freq 2 (/ 3 2))
                        (* freq 2 2)
                        (* freq 2 2 (/ 5 4))
                        (* freq 2 2 (/ 3 2))
                        (* freq 2 2 2)])
        snd   (apply + waves)
        env   (env-gen (adsr a d s r) g 1 0 1 FREE)]
    (* env snd 0.4)))

(defn play-instr [instr metro start-ts end-ts n]
  (if (= n :_ )
    (at (- end-ts 1)   (ctl instr :g 0))
    (do
      (at start-ts (instr (note  n)))
      (at (- end-ts 1)   (ctl instr :g 0)))))

(defn play-phrase
"Play phrase (array of [note duration] using instrument 'instr'"
[instr metro phrase]

(let [notes (map first phrase)
      durs (into [] (map second phrase))
      start-ts  (into [] (map metro (reductions + 0 (map second phrase))))
      phrase-fns (map-indexed
                  (fn [idx n]
                   (play-instr instr metro (start-ts idx) (start-ts (inc idx)) n)) notes)
      ]
  (dorun  phrase-fns)))

(defn play-scale [root mode]
  (let [scale (partition 2 (interleave (scale root mode) (repeat (count (scale root mode)) 1/2)))
        scale-reverse (reverse scale)
        final (reduce (fn [final elem]
                        (conj final elem)) (reverse  scale) (drop 1 (reverse scale)))]
    (play-phrase organ1 (metronome 120) final)))


(defn- parse-int [str]
  (try
    (Integer/parseInt str)
    (catch Exception e
      nil)
    ))

(defn- note-with-octave [note]
  (not (nil? (parse-int (str (last (name note)))))))


(defn with [{:keys [octave tempo] :or {octave 4 tempo 1/2} :as options} notes ]
  (let [
        new-notes (map #(if (note-with-octave  %1)
                         %1
                         (keyword (str (name %1) octave))) notes)
        durs (repeat (count new-notes) tempo)
        vect (->> durs
                  (interleave new-notes)
                  (partition 2 )
                  (map vec)
                  )]
    (into [] vect)))

(def voice1 (let [v  [[:d4 1/4] [:e4 1/4] [:f4 1/4] [:g4 1/4]  [:a4 1/2] [:d5 1/2] [:c#5 1/2] [:a4 1/2] [:e4 1/2] [:g4 1/2]
                      [:f#4 1/2] [:d4 1/2] [:c5 (+ 1 1/2)] [:b4 1/4] [:a4 1/4] [:b4 1/2] [:g4 1/2]
                      [:e4 1/4] [:f4 1/4] [:g4 1/2] [:bb4 1/2] [:d4 1/2] [:c#4 1/2] [:a4 1/2] [:d4 1/2] [:g4 1/2]
                      [:f4 1] [:e4 1] [:d4 1/4] [:c#4 1/4] [:d4 1/4] [:e4 1/4] [:f#4 1/4] [:g#4 1/4] [:a4 1/4] [:b4 1/4]
                      [:a4 1/4] [:b4 1/4] [:c5 1/4] [:d5 1/4] [:e5 1/2] [:a5 1/2] [:g#5 1/2] [:e5 1/2] [:b4 1/2] [:d5 1/2]
                      [:c#5 1/2] [:a4 1/2] [:g5 (+ 1 1/2)] [:f#5 1/4] [:e5 1/4] [:f#5 1/2 ] [:d5 1/2]
                      [:b4 1/4] [:c5 1/4] [:d5 1/2] [:f5 1/2] [:a4 1/2] [:g#4 1/2] [:e5 1/2] [:a4 1/2] [:d5 1/2]
                      [:c5 1] [:b4 1] [:a4 1/4] [:g#4 1/4] [:a4 1/4] [:b4 1/4] [:c5 1/4] [:d5 1/4] [:e5 1/4] [:f5 1/4]
                      [:g5 1/2] [:g4 1/2] [:g5 (+ 1 1/2)] [:e5 1/2] [:a4 1/2] [:g5 1/2]
                      [:f5 1/4] [:e5 1/4] [:d5 1/4] [:e5 1/4] [:f5 1/4] [:e5 1/4] [:f5 1/4] [:g5 1/4] [:a5 (+ 2 1/4)]
                      [:g5 1/4] [:f#5 1/4] [:e5 1/4] [:f#5 1/4] [:d5 1/4] [:e5 1/4] [:f#5 1/4] [:g5 (+ 2 1/4)]
                      [:f5 1/4] [:e5 1/4] [:f5 1/4] [:g5 1/4] [:a5 1/4] [:bb5 1/4] [:g5 1/4] [:a5 1/4] [:bb5 1/4] [:a5 1/4] [:g5 1/4] [:f5 1/4] [:e5 1/4] [:d5 1/4] [:c#5 1/4]
                      [:d5 1/2] [:d6 1] [:c#5 1/2] [:d6 1] [:_ 1/2] [:f5 1/2]
                      [:e5 1/2] [:a5 1/2] [:a4 (+ 1 1/2)]]
                  v (apply conj v (with {:octave 5 :tempo 1/4} [:d :e :f# :g :a :f#]))
                  v (apply conj v [[:g5 1/2]])
                  v (apply conj v (with {:tempo 1/4 :octave 5} [:d :c :d :b4 :c :d]))
                  v (apply conj v [[:g4 1/2] [:c5 1/2] [:g5 (+ 1 1/2)] [:f#5 1/4] [:g5 1/4] [:a5 1/2] [:c5 (+ 1/2 1/2)] [:bb4 1/4] [:c5 1/4] [:d5 1/2] [:a5 1] ])
                  ]

              v))

(defmacro defvoice [name & body]
  `(def ~name
     (reduce conj [] '(~@body))))

(reduce conj [] '([[:a :b :c] [:d :e]] [[:1 :2]]))

(macroexpand '(defvoice ciccio [[:a :b :c] [:d :e]] [[ :d :a :1 :2]]))

(defvoice ciccio [[:a :b :c] [:d :e]] [[:d :a :1 :2]])
ciccio

(defvoice voice2 [[:d2 1/2] [:d3 1/2] [:c3 1/2] [:bb2 1/2] [:a2 1/4] [:g2 1/4] [:a2 1/4]  [:bb2 1/4] [:a2 1/4] [:g2 1/4] [:f2 1/4] [:e2 1/4]
                    [:d2 1/4] [:c2 1/4] [:d2 1/4] [:d#2 1/4] [:d2 1/4] [:c2 1/4] [:b1 1/4]  [:a1 1/4] [:g1 (+ 2 2 1/2) ] [:f1 1/4] [:e1 1/4] [:f1 1/2] [:bb1 1/2]
                    [:a1 1/2] [:g1 1/2] [:a1 1/2] [:a1 1/2] [:d1 1/2] [:d2 1/2] [:c2 1/2] [:b1 1/2]
                    [:a1 1/2] [:a2 1/2] [:g2 1/2] [:f2 1/2] [:e2 1/4] [:d2 1/4] [:e2 1/4] [:f2 1/4] [:e2 1/4] [:d2 1/4] [:c2 1/4] [:b1 1/4]
                    [:a1 1/4] [:g#1 1/4] [:a1 1/4] [:b1 1/4] [:a1 1/4] [:g1 1/4] [:f1 1/4] [:e1 1/4] [:d1 1/4] [:c#2 1/4]  [:d2 1/4] [:e2 1/4] [:d2 1/4] [:c2 1/4] [:b1 1/4] [:a1 1/4]
                    [:g#1 1] [:_ 1/2] [:d2 (+ 1/2 1/2)] [:c2 1/4] [:b1 1/4] [:c2 1/2] [:f2 1/2] [:e2 1/2] [:d2 1/2] [:e2 1/2] [:e1 1/2] [:a1 1] [:_ 1]
                    [:_ 4] [:d2 1/4] [:e2 1/4] [:f2 1/4] [:g2 1/4] [:a2 1/2] [:d3 1/2] [:c#3 1/2] [:a2 1/2] [:e2 1/2] [:g2 1/2]
                    [:f#2 1/2] [:d2 1/2] [:c3 (+ 1 1/2)] [:b2 1/4] [:a2 1/4] [:b2 1/2] [:g2 1/2]
                    [:e2 1/4] [:f2 1/4]]
                 (with {:octave 2 :tempo 1/2} [:g :bb :d :c# :a :d :g ])
                 [[:f2 1] [:e2 1]]
                 (with {:octave 2 :tempo 1/4} [:d :c# :d :e :f :g :a :b :c#3 :b :a :b :c3 :a :b :c3]))
voice2
(let [metro (metronome 90)]                                        ;
  (play-phrase organ1 metro  voice1)

 (play-phrase organ2 metro  voice2)
 )

(stop)

(play-scale :d4 :major)
