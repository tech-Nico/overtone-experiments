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
(organ1 :a 0)
(stop)
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

(defn- play-silence [instr at-ts]
  (at at-ts (ctl instr :g 0)))

(defn play-instr [instr metro start-ts end-ts n {:keys [staccato] :or {staccato false} :as style}]
(println style)
  (if (= n :_ )
    (play-silence instr (- end-ts 1))
    (do
      (if staccato
        (do
          (at start-ts (instr (note n) {:d 0 :r 0.2}))
          (play-silence instr (+ start-ts (/ (- end-ts start-ts) 2))))
        (do
          (at start-ts (instr (note  n)))
          (play-silence instr (- end-ts 1)))))))

(defn play-phrase
"Play phrase (array of [note duration] using instrument 'instr'"
[instr metro phrase]

(let [notes (map first phrase)
      durs (into [] (map second phrase))
      styles (into [] (map #(nth % 2 {}) phrase))
      start-ts  (into [] (map metro (reductions + 0 (map second phrase))))
      phrase-fns (map-indexed
                  (fn [idx n]
                    (println "The style of " idx " is " (styles idx))
                    (play-instr instr metro (start-ts idx) (start-ts (inc idx)) n  (styles idx))) notes)]
  (println "The styles" styles)
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
      nil)))

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

(defmacro defvoice
  "Basically defines an array of notes and give it a name. The 'cool' thing is that the array can be composed because
you can build some of the array's component by calling functions"
  [name & body]
  `(def ~name
     (reduce (fn [coll# el#]
               (if (list? el#)
                 (apply conj coll# (eval el#) )
                 (conj coll# (eval  el#)))) [] '(~@body))))


(defvoice voice1 [:d4 1/4] [:e4 1/4] [:f4 1/4] [:g4 1/4]  [:a4 1/2 {:staccato true}] [:d5 1/2 {:staccato true}] [:c#5 1/2] [:a4 1/2] [:e4 1/2] [:g4 1/2]
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
                 [:e5 1/2] [:a5 1/2] [:a4 (+ 1 1/2)]
                 (with {:octave 5 :tempo 1/4} [:d :e :f# :g :a :f#])
                 [:g5 1/2]
                 (with {:tempo 1/4 :octave 5} [:d :c :d :b4 :c :d])
                 [:g4 1/2] [:c5 1/2] [:g5 (+ 1 1/2)] [:f#5 1/4] [:g5 1/4] [:a5 1/2] [:c5 (+ 1/2 1/2)] [:bb4 1/4] [:c5 1/4] [:d5 1/2] [:a5 1]
                 [:g5 1] [:f5 1] (with {:octave 5 :tempo 1/4} [:e :d :c# :d :b4 :c# :d :e :f :g])
                 (with {:tempo 1/2 :octave 5} [:a :d6 :c#6 :a :e :g :f# :d])  [:c6 (+ 1 1/2)] [:b5 1/4] [:a5 1/4]
                 (with {:tempo 1/2 :octave 5} [:b :g :e :g :bb :d :c# :a :d :g]) [:f5 1] [:e5 1] [:d5 1/2]
                 (with {:tempo 1/4 :octave 4} [:c5 :bb :a :g :f :e])
                 (with {:tempo 1/2 :octave 5} [:f4 :a :e4 :g]) (with {:tempo 1/4 :octave 5} [:d3 :g :f :e :d :c :bb4 :a4])
                 (with {:tempo 1/2 :octave 4} [:bb :d6 :a :c6])
                 (with {:tempo 1/4 :octave 5} [:g3 :c6 :bb :a :g :f :e :d :e :bb :a :g :a :e :d :c# :d :a :g :f :g :d :c# :b4 :c# :g :f :e :f :e :d :f :e :d :c# :b4])
                 [:a4 (+ 1 1/4)]

                 )


(defvoice voice2 [:d2 1/2] [:d3 1/2] [:c3 1/2] [:bb2 1/2] [:a2 1/4] [:g2 1/4] [:a2 1/4]  [:bb2 1/4] [:a2 1/4] [:g2 1/4] [:f2 1/4] [:e2 1/4]
                 [:d2 1/4] [:c2 1/4] [:d2 1/4] [:d#2 1/4] [:d2 1/4] [:c2 1/4] [:b1 1/4]  [:a1 1/4] [:g1 (+ 2 2 1/2) ] [:f1 1/4] [:e1 1/4] [:f1 1/2] [:bb1 1/2]
                 [:a1 1/2] [:g1 1/2] [:a1 1/2] [:a1 1/2] [:d1 1/2] [:d2 1/2] [:c2 1/2] [:b1 1/2]
                 [:a1 1/2] [:a2 1/2] [:g2 1/2] [:f2 1/2] [:e2 1/4] [:d2 1/4] [:e2 1/4] [:f2 1/4] [:e2 1/4] [:d2 1/4] [:c2 1/4] [:b1 1/4]
                 [:a1 1/4] [:g#1 1/4] [:a1 1/4] [:b1 1/4] [:a1 1/4] [:g1 1/4] [:f1 1/4] [:e1 1/4] [:d1 1/4] [:c#2 1/4]  [:d2 1/4] [:e2 1/4] [:d2 1/4] [:c2 1/4] [:b1 1/4] [:a1 1/4]
                 [:g#1 1] [:_ 1/2] [:d2 (+ 1/2 1/2)] [:c2 1/4] [:b1 1/4] [:c2 1/2] [:f2 1/2] [:e2 1/2] [:d2 1/2] [:e2 1/2] [:e1 1/2] [:a1 1] [:_ 1]
                 [:_ 4] [:d2 1/4] [:e2 1/4] [:f2 1/4] [:g2 1/4] [:a2 1/2] [:d3 1/2] [:c#3 1/2] [:a2 1/2] [:e2 1/2] [:g2 1/2]
                 [:f#2 1/2] [:d2 1/2] [:c3 (+ 1 1/2)] [:b2 1/4] [:a2 1/4] [:b2 1/2] [:g2 1/2]
                 [:e2 1/4] [:f2 1/4]
                 (with {:octave 2 :tempo 1/2} [:g :bb :d :c# :a :d :g ])
                 [:f2 1] [:e2 1]
                 (with {:octave 2 :tempo 1/4} [:d :c# :d :e :f :g :a :b :c#3 :b :a :b :c#3 :a :b :c#3]) [:d3 (+ 2 1/4)]
                 (with {:octave 2 :tempo 1/4} [:c3 :b :a :b :g :a :b ]) [:c3 (+ 2 1/4)]
                 (with {:octave 2 :tempo 1/4} [:bb :a :bb :c3 :d3 :d#3 :c3 :d3 :d#3 :d3 :c3 :bb :a :g :f#])
                 (with {:octave 2 :tempo 1/4} [:g :f :e :d :c# :a1 :d :c :bb1 :d :c# :d :e :f :g :e :f :e :d :e :f :e :f :g] ) [:a2 (+ 2 1/4)]
                 (with {:octave 2 :tempo 1/4} [:g :f# :e :f# :d :e :f#]) [:g2 ( + 2 1/4)]
                 (with {:octave 2 :tempo 1/4} [:f :e :f :g :a :bb :g :a :bb :a :g :f :e :d :c# ])
                 (with {:octave 1 :tempo 1/2} [:d2 :bb :g :a ]) [:d1 1] [:_ 1]
                 [:d2 1/2] [:_ 1/2] [:a1 1/2] [:_ 1/2] [:d1 1/2 ] [:_ (+ 1 1/2)]
                 [:g1 1/2] [:_ 1/2] [:d2 1/2] [:_ 1/2] [:g2 1/2] [:_ (+ 1 1/2)]
                 [:c3 1/2] [:_ 1/2] [:f2 1/2] [:_ 1/2] [:bb2 1/2] [:_ 1/2] [:e2 1/2] [:_ 1/2]
                 [:a2 1/2] [:_ 1/2] [:d2 1/2] [:_ 1/2] (with {:tempo 1/2 :octave 2} [:a :e :c# :a1])


                 )

(defvoice test
  (with {:tempo 1/4 :octave 5} [:g3 :c6 :bb :a :g :f :e :d :e :bb :a :g :a :e :d :c# :d :a :g :f :g :d :c# :b4 :c# :g :f :e :f :e :d :f :e :d :c# :b4])
  [:a4 (+ 1 1/4)])

(let [metro (metronome 90)
      ]
  (play-phrase organ1 metro test))

(let [metro (metronome 90)]                                        ;

  (play-phrase organ1 metro  voice1)

  (play-phrase organ2 metro  voice2)
  )

(stop)

(play-scale :d4 :major)
