(ns brainfudje.core
  (:gen-class))

(defn find-char 
  [input curr cha func]
  (cond
   (>= curr (count input)) (count input)
   (< curr 0) 0
   (= (nth input curr) cha) (inc curr) 
   :else (recur input (func curr) cha func)))

(defn bf-interp [input instr d-array ptr]
  (let [top (nth input instr -1)
        cell (nth d-array ptr)]
    (cond
     (= -1 top) (list d-array ptr) ; if the instruction pointer is out-of-bounds return
     (= top \>) (cond
                 (= ptr (dec (count d-array))) (recur input (inc instr) d-array ptr)
                 :else (recur input (inc instr) d-array (inc ptr)))
     (= top \<) (cond
                 (= ptr 0) (recur input (inc instr) d-array ptr)
                 :else (recur input (inc instr) d-array (- ptr 1)))
     (= top \+) (recur input (inc instr) (assoc d-array ptr (mod (inc cell) 256)) ptr)
     (= top \-) (recur input (inc instr) (assoc d-array ptr (mod (dec cell) 256)) ptr)
     (= top \,) (let [in (int (first (read-line)))]
                  (flush)
                  (recur input (inc instr) (assoc d-array ptr (mod (+ cell in) 256)) ptr))
     (= top \[) (if (= 0 (nth d-array ptr)) (recur input (find-char input instr \] inc) d-array ptr)
                    (recur input (inc instr) d-array ptr))
     (= top \]) (if (not= 0 (nth d-array ptr)) (recur input (find-char input instr \[ dec) d-array ptr)
                     (recur input (inc instr) d-array ptr))
     (= top \.) (do
                   (println (char (nth d-array ptr)))
                   (flush)
                   (recur input (inc instr) d-array ptr))
     :else (recur input (inc instr) d-array ptr))))

(defn bf-repl []
  (loop [d-array (vec (take 30000 (repeat 0)))
         ptr 0]
    (do
      (print "brainfudje> ")
      (flush)
      (let [input (read-line)
            out-tup (bf-interp input 0 d-array ptr)]
        (recur (first out-tup) (second out-tup))))))

(defn -main [& args]
  (bf-repl))