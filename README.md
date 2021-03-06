# Cheshire

<img src="http://dakrone.github.com/cheshire/cheshire_small.jpg"
title=":)" align="left">
<small>
'Cheshire Puss,' she began, rather timidly, as she did not at all know
whether it would like the name: however, it only grinned a little
wider.  'Come, it's pleased so far,' thought Alice, and she went
on. 'Would you tell me, please, which way I ought to go from here?'

'That depends a good deal on where you want to get to,' said the Cat.

'I don't much care where--' said Alice.

'Then it doesn't matter which way you go,' said the Cat.

'--so long as I get SOMEWHERE,' Alice added as an explanation.

'Oh, you're sure to do that,' said the Cat, 'if you only walk long
enough.'
</small>
</img>
<br clear=all /><br />
Cheshire is fast JSON encoding, based off of clj-json and
clojure-json, with additional features like Date/UUID/Set/Symbol
encoding and SMILE support.

[Clojure code with docs](http://dakrone.github.com/cheshire/)

## Why?

clojure-json had really nice features (custom encoders), but was slow;
clj-json had almost no features, but was very fast. I wanted the best
of both worlds.

## Usage

    [cheshire "1.1.1"]

### Encoding

    ;; generate some json
    (generate-string {:foo "bar" :baz 5})

    ;; write some json to a stream
    (generate-stream (clojure.java.io/writer "/tmp/foo") {:foo "bar" :baz 5})

    ;; generate some SMILE
    (generate-smile {:foo "bar" :baz 5})

    ;; generate some JSON with Dates
    ;; the Date will be encoded as a string using
    ;; the default date format: yyyy-MM-dd'T'HH:mm:ss'Z'
    (generate-string {:foo "bar" :baz (Date. 0)})

    ;; generate some JSON with Dates with custom Date encoding
    (generate-string {:baz (Date. 0)} "yyyy-MM-dd")

### Decoding

    ;; parse some json
    (parse-string "{\"foo\":\"bar\"}")
    ;; => {"foo" "bar"}

    ;; parse some json and get keywords back
    (parse-string "{\"foo\":\"bar\"}" true)
    ;; => {:foo "bar"}

    ;; parse some SMILE (keywords option also supported)
    (parse-smile <your-byte-array>)

    ;; parse a stream (keywords option also supported)
    (parse-stream (clojure.java.io/reader "/tmp/foo"))

    ;; parse a stream lazily (keywords option also supported)
    (parsed-seq (clojure.java.io/reader "/tmp/foo"))

    ;; parse a SMILE stream lazily (keywords option also supported)
    (parsed-smile-seq (clojure.java.io/reader "/tmp/foo"))

There are also a few aliases for commonly used functions:

    encode -> generate-string
    encode-stream -> generate-stream
    encode-smile -> generate-smile
    decode -> parse-string
    decode-stream -> parse-stream
    decode-smile -> parse-smile

## Features
Cheshire supports encoding standard clojure datastructures, but with a
few addons. Right now it does not support custom encoders, but a few
helpers were added to support commonly encoded classes.

Cheshire encoding supports:

### Clojure data structures
- strings
- lists
- vectors
- sets (clj-json does not yet support sets)
- maps
- symbols (clj-json does not yet support symbols)
- booleans
- numbers (Integer, Long, BigInt, Double, Float)

### Java classes
- Date
- UUID

### Also supports
- Stream encoding/decoding
- Lazy decoding
- [SMILE encoding/decoding](http://wiki.fasterxml.com/SmileFormatSpec)

Work is underway to have custom object encoders while still being fast.

## Speed

    Clojure version:  1.2.1
    Num roundtrips:   100000

    Trail:  1
    clj-json                               2.16
    clj-json w/ keywords                   2.43
    clj-serializer                         2.13
    cheshire                               2.19
    cheshire-smile                         2.20
    cheshire w/ keywords                   2.12
    clojure printer/reader                 7.16
    clojure printer/reader w/ print-dup    12.29
    clojure-json                           20.55
    
    Trail:  2
    clj-json                               1.23
    clj-json w/ keywords                   2.17
    clj-serializer                         1.58
    cheshire                               1.49
    cheshire-smile                         1.49
    cheshire w/ keywords                   1.99
    clojure printer/reader                 5.97
    clojure printer/reader w/ print-dup    11.17
    clojure-json                           20.42

Cheshire is right up there with clj-json.

## Future Ideas/TODOs
- move away from using Java entirely, use Protocols for the encoder
- allow custom encoders (this should be doable once switched over to protocols)

## License
Release under the MIT license. See LICENSE for the full license.

## Thanks
Thanks go to Mark McGranaghan for allowing me to copy the clj-json
code to get started on this and Jim Duey for the name. :)
