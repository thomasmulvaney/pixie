(ns pixie.tar
  (require pixie.streams :as st :refer :all)
  (require pixie.io-blocking :as io))

(def HEADER_SIZE 512)
(def BLOCK_SIZE 512)

(def char->num
  {0 0
   \0 0
   \1 1
   \2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7})

(defn octal-sum [s]
  (first (reduce (fn [[sum oct] c]
           [(+ sum (* (char->num c) oct)) 
            (* oct 8)])
          [0 1]
          (reduce conj nil (seq s)))))

(defn file-padding [filesize]
  (let [r (rem filesize BLOCK_SIZE)
        q (quot filesize BLOCK_SIZE)
        res (cond
              (zero? filesize) 0
              (= filesize BLOCK_SIZE) 0
              (pos? r)         (- (* (inc q) BLOCK_SIZE) filesize)
              :else 0)]
    (int res)))

(def fields
  [[:file-name        100]
   [:file-mode          8]
   [:owner              8]
   [:group              8]
   [:file-size-octal   12]
   [:modified          12]
   [:checksum           8]
   [:type               1]
   [:linkname         100]
   [:ustar              6]
   [:ustar-version      2]
   [:owner-user        32]
   [:owner-group       32]
   [:device-major       8]
   [:device-minor       8]
   [:file-name-prefix 155]])

(defn read-padded-buffer 
  "Buffer is filled with 0s so ignore them"
  [buffer]
  (->> buffer
      (reduce conj [])
      (take-while #(not= 0 %))
      (map char)
      (apply str)))

(defn read-header 
  [file-stream buf pos]
  (let [[tarhead _] (reduce 
                      (fn [[header pos] [field size]]
                        (io/read file-stream buf size)
                        [(assoc header field (read-padded-buffer buf)) (+ pos size)])
                      [{} 0]
                      fields)
        tarfile-length (tarhead :file-size-octal)]
    (if (empty? (:file-name tarhead))
      nil
      (assoc tarhead 
             :file-pos  (+ HEADER_SIZE pos)
             :file-size (octal-sum tarfile-length)
             :file-padding (file-padding ( octal-sum tarfile-length))))))

(defn skip-file [file-stream header]
  (let [{:keys [file-pos file-size file-padding]} header]
    (io/seek file-stream
             (int (+ file-pos file-size file-padding)))))


;; TODO make a lazy version of this
(defn list-files [file-stream]
  (let [buf (buffer HEADER_SIZE)]
    (loop [x 0 pos 0 headers []] 
      (if-let [header (read-header file-stream buf pos)]
        (let [{:keys [file-size file-padding file-pos]} header]
          ;; We are just extracting the headers so we need to skip the
          ;; file itself
          (skip-file file-stream header)
          (recur (inc x) 
                 (int (+ pos file-size file-padding HEADER_SIZE))
                 (conj headers header)))
        headers))))

;; A tarfile is a a file pointer and the metadata to locate the
;; contents with in the file
(deftype TarFile [fp header]
  IInputStream
  (read [this buffer len]
    (assert (>= (buffer-capacity buffer) len)
            "Not enough capacity in the buffer")

    ;; Move the filepointer to the beginning of the tar file
    (when (zero? (position this))
      (rewind this))

    ;; If we are not at the end of the tarfile
    (if (> (+ (:file-pos header) (:file-size header)) 
           (position this))
      (let [read-count (io/fread buffer 1 len fp)]
        (set-buffer-count! buffer read-count)
        read-count)

      ;; We hit the end so we read nothing
      0))

  (read-byte [this]
    ;; Move the filepointer to the beginning of the tar file
    (when (zero? (position this))
      (rewind this))

    (assert (>= (+ (:file-pos header) (:file-size header)) pos) "Can't seek past the tarfile")
    (assert (>=  (:file-pos header) pos) "Can't read before the tarfile")

    (if (> (+ (:file-pos header) (:file-size header)) 
           (position this)) 
      (io/fgetc buffer)
      0))

    ISeekableStream
    (position [this]
              (io/ftell fp))

    (seek [this pos]
          (assert (>= (+ (:file-pos header) (:file-size header)) pos) "Can't seek past the tarfile")
          (assert (>=  (:file-pos header) pos) "Can't seek before the tarfile")
          (io/fseek fp pos 0))

    ;; Normally rewind would take you back to zero. We want the
    ;; beginning of the file inside the tar
    (rewind [this]
            (io/fseek fp (:file-pos header) 0))

    IDisposable
    (-dispose! [this]
               (io/pclose fp))

    (-reduce [this f init]
             (io/default-stream-reducer this f init)))

;; Print the contents of a TAR archive
(doseq [header (list-files (io/open-read "tests.tar"))]
  (println "Filename: "   (:file-name header))
  (println "First line: " (io/read-line
                            (->TarFile (io/fopen "tests.tar" "r") header))))
