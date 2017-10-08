(ns texcompiler.core
  (:gen-class))

(defn is-cs-char
  "test if a char is valid for control sequence"
  [c]
  (or (<= (int \a) (int c) (int \z))
      (<= (int \A) (int c) (int \Z))))

(defn next-state
  "Get next state based on current state and input char.
   - :s start
   - :s-cs constrol sequence
   - :s-group start of grouping \{
   - :e-group end of grouping \}
   - :s-space \space
   - :text normal text
   - :s-group' same as s-group, used for consequtive \{
   - :e-group' same as e-group, used for consequtive \}
  "
  [cs buf c]
  (cond (= :s cs) (cond (= \\ c) :s-cs
                        (= \{ c) :s-group
                        (= \} c) :e-group
                        :else :text)
        (= :s-cs cs) (cond (and (is-cs-char c) (not (= \space (last buf)))) :s-cs
                           (= \space c) (if (empty? (rest buf)) :s-cs :s-space)
                           (= \{ c) :s-group
                           (= \} c) :e-group
                           :else :text)
        (= :s-group cs) (cond (= \\ c) :s-cs
                              (= \{ c) :s-group'
                              (= \} c) :e-group
                              (= \space c) :s-space
                              :else :text)
        (= :e-group cs) (cond (= \\ c) :s-cs
                              (= \{ c) :s-group
                              (= \} c) :e-group'
                              (= \space c) :s-space
                              :else :text)
        (= :s-group' cs) (cond (= \\ c) :s-cs
                               (= \{ c) :s-group
                               (= \} c) :e-group
                               (= \space c) :s-space
                               :else :text)
        (= :e-group' cs) (cond (= \\ c) :s-cs
                               (= \{ c) :s-group
                               (= \} c) :e-group
                               (= \space c) :s-space
                               :else :text)
        (= :text cs) (cond (= \\ c) :s-cs
                           (= \{ c) :s-group
                           (= \} c) :e-group
                           :else :text)
        (= :s-space cs) (cond (= \space c) :s-space
                              (= \\ c) :s-cs
                              (= \{ c) :s-group
                              (= \} c) :e-group
                              :else :text)))

(defn next-char
  [state buf c]
  (let [new-state (next-state state buf c)
        new-buf (if (= new-state state) (conj buf c) [c])]
    [new-state new-buf]))

(defn next-token
  [state buf s]
  (if (empty? s)
    [state [] s (if (empty? buf) nil [state buf])]
    (let [x (first s)
          [new-state new-buf]
          (next-char state buf x)]
      (if (= new-state state)
        (next-token new-state new-buf (rest s))
        [new-state new-buf (rest s) [state buf]]))))

(defn token-seq
  "return a lazy-seq of tokens"
  ([s]
   (token-seq :s [] s))

  ([state buf s]
   (lazy-seq
    (let [[new-state new-buf s' token] (next-token state buf s)]
      (if (nil? token)
        ()
        (cons token (token-seq new-state new-buf s')))))))

(defn all-tokens
  "tokenize tex string
  "
  [s]
  (let [[state buf tokens]
        (reduce
         (fn [[state buf tokens] x]
           (let [[new-state new-buf]
                 (next-char state buf x)]
             [new-state new-buf
              (if (= new-state state) tokens (conj tokens [state buf]))]))
         [:s [] []] s)]
    (rest
     (if (empty? buf)
       tokens
       (conj tokens [state buf])))))

(defn show-tokens
  [tokens]
  (pprint (map (fn [[tt t]] [tt (apply str t)]) tokens)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
