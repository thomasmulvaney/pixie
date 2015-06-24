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
  (empty-output? [this]
    "Returns true if there is no deflated (compressed) data in the output")
  (full-output? [this]
    "Returns true if the ouput buffer is full of deflated (compressed) data")
  (reset-output-buffer! [this]
    "Make the output buffer ready to be refilled with data")
  
  (set-output-buffer-count! [this]
    "Set the buffers count so down stream can safely read the buffer")

  ;; Deflation
  (deflate-init! [this opts]
    "Set up the compression with desired parameters")
  (deflate! [this down-stream mode]
    "Deflates the input buffer making one or more writes of compressed
    input to down stream")
  (deflate-end! [this]
    "Cleanup")
  
  ;; Inflation
  (inflate-init! [this opts])
  (inflate! [this down-stream mode])
  (inflate-end!)

  ;; In/deflate depending on what the stream has been
  ;; initialized as
  (flate! [this down-stream mode])
  
  (total-in [this])
  (total-out [this]))

(deftype ZStream [z-stream output-buffer inited]
  IZStream
  (version [this]
    (zlibVersion))

  (full-output? [this]
    (zero? (get z-stream :avail_out)))

  (empty-output? [this]
    (zero? (count output-buffer)))

  (reset-output-buffer! [this]
    (ffi/set! z-stream :next_out  output-buffer)
    (ffi/set! z-stream :avail_out (buffer-capacity output-buffer)))

  (set-output-buffer-count! [this]
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

  (inflate! [this down-stream mode]
    (assert (= :inflate inited) "ZStream must be initialized before deflation")
    (println "inflate")
    (loop [deflate-cycles 0]
      (reset-output-buffer! this)
      (let [status (inflate z-stream mode)]
        (set-output-buffer-count! this)
        (when-not (empty-output? this)
          (write down-stream output-buffer))
        (when (and (full-output? this) (= status Z_OK))
          (recur (inc deflate-cycles))))))

  (flate! [this down-stream mode]
    (if (= :deflate inited)
      (deflate! this down-stream mode)
      (inflate! this down-stream mode)))

  (deflate! [this down-stream mode]
    (assert (= :deflate inited) "ZStream must be initialized before deflation")
    (loop [deflate-cycles 0]
      (reset-output-buffer! this)
      (let [status (deflate z-stream mode)]
        (set-output-buffer-count! this)
        (when-not (empty-output? this)
          (write down-stream output-buffer))
        ;; If the buffer can be refilled, do it.
        (when (and (full-output? this) (= status Z_OK))
          (recur (inc deflate-cycles)))))))

(defn z-stream [output-buffer]
  (let [z (z_stream)]
    (ffi/set! z :zalloc nil)
    (ffi/set! z :zfree nil)
    (ffi/set! z :opaque nil)
  (->ZStream z output-buffer nil)))

(defprotocol ICodecStream
  (bytes-in  [this])
  (bytes-out [this])
  (ratio     [this]))

(defprotocol IZlibCodecStream
  (set-inflate! [this opts])
  (set-deflate! [this opts]))

(deftype ZlibCodecStream 
  [down-stream z-stream]
  IOutputStream
  (write [this input-buffer]
    (set-input! z-stream input-buffer)
    (flate! z-stream down-stream Z_NO_FLUSH))

  IFlushableStream
  (flush [this]
    (flate! z-stream down-stream Z_FINISH)
    (when (satisfies? IFlushableStream down-stream)
      (flush down-stream)))

  IDisposable
  (-dispose! [this]
    (deflate-end! z-stream)
    (when (satisfies? IDisposable down-stream)
      (-dispose! down-stream)))
  
  IZlibCodecStream
  (set-inflate! [this opts]
    (inflate-init! z-stream opts)
    this)
  
  (set-deflate! [this opts]
    (deflate-init! z-stream opts)
    this))

(defn compressing-stream 
  "Takes a down-stream IInputStream and a buffer to store compressed chunks"
  [down-stream output-buffer]
  (let [z-stream (z-stream output-buffer)]
    (-> down-stream
        (->ZlibCodecStream z-stream)
        (set-deflate! {}))))

(defn decompressing-stream 
  "Takes a down-stream IInputStream and a buffer to store compressed chunks"
  [down-stream output-buffer]
  (let [z-stream (z-stream output-buffer)]
    (-> down-stream
        (->ZlibCodecStream z-stream)
        (set-inflate! {}))))

(defn test3 [stream content]
  (transduce (map identity)
             (-> 
               stream
               (compressing-stream (buffer 128))
               (io/buffered-output-stream 1024)
               utf8/utf8-output-stream-rf)
             (str content)))

(defn test2 [stream content]
  (transduce (map identity)
             (-> 
               stream
               (decompressing-stream (buffer 128))
               (compressing-stream (buffer 128))
               (io/buffered-output-stream 1024)
               utf8/utf8-output-stream-rf)
             (str content)))
