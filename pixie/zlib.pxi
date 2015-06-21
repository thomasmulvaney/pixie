(ns pixie.zlib
  (:require [pixie.ffi-infer :as f]
            [pixie.streams :refer :all]
            [pixie.io :as io]
            [pixie.streams.utf8 :as utf8]
            [pixie.ffi :as ffi]))

(f/with-config  {:library "z"
                :includes ["zlib.h"]}
  (f/defcstruct z_stream [:next_in 
                          :avail_in 
                          :total_in
                          :next_out 
                          :avail_out
                          :total_out

                          :msg
                          :state

                          :zalloc 
                          :zfree 
                          :opaque 

                          :data_type
                          :adler

                          :reserved])

  (f/defcfn zError)
  (f/defcfn zlibVersion)

  ;; Inflating (decompressing)
  (f/defcfn inflate)
  (f/defcfn inflateEnd)
  (f/defcfn inflateInit2_)

  ;; Defalting (compressing)
  (f/defcfn deflate)
  (f/defcfn deflateInit_)
  (f/defcfn deflateInit2_)
  (f/defcfn deflateEnd))

(def Z_OK 0)
(def Z_NO_FLUSH 0)
(def Z_PARTIAL_FLUSH 1)
(def Z_SYNC_FLUSH 2)
(def Z_FULL_FLUSH 3)
(def Z_FINISH 4)
(def Z_BLOCK 5)
(def Z_TREES 4)

(def Z_NO_COMPRESSION 0)
(def Z_BEST_SPEED 1)
(def Z_BEST_COMPRESSION 9)
(def Z_DEFAULT_COMPRESSION -1)

(def Z_DEFLATED 8)

(def Z_DEFAULT_STRATEGY 0)

(defprotocol IZStream
  (version [this])
  (set-input! [this]
    "Should be called before deflate! to set a new chunk of input
    to deflate")
  (full-output? [this]
    "Returns true if the ouput buffer is full of deflated (compressed) data")
  (reset-output-buffer! [this]
    "Make the output buffer ready to be refilled with data")
 
  (consumed-input? [this]
    "Returns true if zlib has finished reading the input-buffer") 

  (set-output-buffer-count! [this]
    "Set the buffers count so down stream can safely read the buffer")

  ;; Deflation
  (deflate-init! [this opts]
    "Set up the compression with desired parameters")

  ;; Inflation
  (inflate-init! [this opts]
    "Set up decompression with desired parameters")

  ;; In/deflate depending on what the stream has been
  ;; initialized as
  (flate! [this down-stream mode]
    "Compress/decompress")

  (flate-end!
    "Cleanup")
  
  (total-in [this])
  (total-out [this]))

(deftype ZStream [z-stream inited]
  IZStream
  (version [this]
    (zlibVersion))

  (full-output? [this]
    (zero? (get z-stream :avail_out)))

  (consumed-input? [this]
    (zero? (get z-stream :avail_in)))

  (reset-output-buffer! [this output-buffer]
    (ffi/set! z-stream :next_out  output-buffer)
    (ffi/set! z-stream :avail_out (buffer-capacity output-buffer)))

  (set-output-buffer-count! [this output-buffer]
    (let [fill-count (- (buffer-capacity output-buffer) 
                        (get z-stream :avail_out))]
      (set-buffer-count! output-buffer fill-count)))

  (set-input! [this input-buffer]
    (ffi/set! z-stream :next_in input-buffer)
    (ffi/set! z-stream :avail_in (count input-buffer)))

  (deflate-init! [this opts]
    (assert (nil? inited) "ZStream can only be initialized once.")
    (let [status (deflateInit2_ 
                   z-stream 
                   (get opts :level Z_BEST_COMPRESSION) ;level
                   Z_DEFLATED         ;method
                   (+ 15 16)          ;window (set for gz header)
                   8                  ;memlevel
                   Z_DEFAULT_STRATEGY ;strategy
                   (version this)    ;version
                   (ffi/struct-size z_stream))]
      (assert (= Z_OK status) "Failed to initiate zstream")
      (set-field! this :inited :deflate)))

  (inflate-init! [this opts]
    (assert (not inited) "ZStream can only be initialized once.")
    (let [status (inflateInit2_ 
                   z-stream 
                   (+ 15 16)          ;window (set for gz header)
                   (version this)    ;version
                   (ffi/struct-size z_stream))]
      (assert (= Z_OK status) "Failed to initiate zstream")
      (set-field! this :inited :inflate)))

  (flate! [this output-buffer mode]
    (cond
      (= :inflate inited)
      (do
      (let [status (inflate z-stream mode)]
        (set-output-buffer-count! this output-buffer)
        status))

      (= :deflate inited)
      (do
        (let [status (deflate z-stream mode)]
          (set-output-buffer-count! this output-buffer)
          status))

      :else
      (assert false "ZStream must be initialized before calling flate!")))
  
  (flate-end! [this]
    (cond
      (= :inflate inited)
      (inflateEnd z-stream)
      
      (= :deflate inited)
      (deflateEnd z-stream)
     
      :else
      (assert false "ZStream must be initialized before calling flate-end!"))))

(defn z-stream []
  (let [z-stream (z_stream)]
    ;; don't set any callbacks
    (ffi/set! z-stream :avail_in 0)
    (ffi/set! z-stream :zalloc nil)
    (ffi/set! z-stream :opaque nil)
    (ffi/set! z-stream :zfree nil)
    (->ZStream z-stream nil)))

(defprotocol ICodecStream
  (bytes-in  [this])
  (bytes-out [this])
  (ratio     [this]))

(defn buffer->str [buf]
  (loop [n 0 acc []]
    (if (not (= (count buf) n))
      (recur (inc n) (conj acc (char (nth buf n))))
      (apply str acc))))

(deftype ZlibCodecInputStream
  [up-stream input-buffer z-stream]
  IDisposable
  (-dispose! [this]
    (flate-end! z-stream))

  IInputStream
  (read [this buffer len]
    (set-buffer-count! buffer 0)
    (reset-output-buffer! z-stream buffer)
    (loop [z 0]
      (when (consumed-input? z-stream)
        ;; If z-stream has finished reading the input-buffer we last gave it,
        ;; give it a new one.
        (read up-stream input-buffer (buffer-capacity input-buffer))
        (set-input! z-stream input-buffer))
      (if-not (zero? (count input-buffer))
        (do
          (let [status (flate! z-stream buffer Z_NO_FLUSH)]
            (cond
              ;; Go through and handle all status's
              ;(= -3 status) (throw [::DataException "Data is not zlib/gzip format"])
             
              (neg? status) 
              (throw [::Exception "something went wrong."]))
            (assert (not (neg? status)) "An error has occured."))
          (if (or (full-output? z-stream) (zero? (count buffer)))
            ;; The buffer is now filled up
            (count buffer)

            ;; We can still do some more de/compression
            (recur (inc z))))
        0))))

(deftype ZlibCodecOutputStream
  [down-stream output-buffer z-stream]
  IOutputStream
  (write [this input-buffer]
    (set-input! z-stream input-buffer)
    (loop [z 0]
      (reset-output-buffer! z-stream output-buffer)
      (let [status (flate! z-stream output-buffer Z_NO_FLUSH)]
        (when-not (zero? (count output-buffer))
          (write down-stream output-buffer))
        ;; if there is still more 'flating to do, do it.
        (when (and (full-output? z-stream) (= status Z_OK))
          (recur (inc z))))))

  IFlushableStream
  (flush [this]
    (loop [z 0]
      (reset-output-buffer! z-stream output-buffer)
      (let [status (flate! z-stream output-buffer Z_FINISH)]
        (write down-stream output-buffer))
        ;; if there is still more 'flating to do, do it.
        (when (and (full-output? z-stream) (= Z_FINISH))
          (recur (inc z))))
    (when (satisfies? IFlushableStream down-stream)
      (flush down-stream)))

  IDisposable
  (-dispose! [this]
    (flate-end! z-stream)
    (when (satisfies? IDisposable down-stream)
      (-dispose! down-stream))))

(defn compressing-output-stream 
  "Takes a down-stream IInputStream and a buffer to store compressed chunks"
  [down-stream output-buffer opts]
  (let [z-stream (z-stream)]
    (->ZlibCodecOutputStream down-stream output-buffer (deflate-init! z-stream opts))))

(defn decompressing-output-stream 
  "Takes a down-stream IInputStream and a buffer to store compressed chunks"
  [down-stream output-buffer opts]
  (let [z-stream (z-stream)]
    (->ZlibCodecOutputStream down-stream output-buffer (inflate-init! z-stream opts))))

(defn decompressing-input-stream 
  "Takes a down-stream IInputStream and a buffer to store compressed chunks"
  [up-stream input-buffer opts]
  (let [z-stream (z-stream)]
    (->ZlibCodecInputStream up-stream input-buffer (inflate-init! z-stream opts))))

(defn compressing-input-stream 
  "Takes a down-stream IInputStream and a buffer to store compressed chunks"
  [up-stream input-buffer opts]
  (let [z-stream (z-stream)]
    (->ZlibCodecInputStream up-stream input-buffer (deflate-init! z-stream opts))))
