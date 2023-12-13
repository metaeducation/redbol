Rebol [
    Title: "Emulation of Rebol2 CLOAK/DECLOAK"
    File: %cloak2.r
    License: {LGPL 3.0}

    Type: module
    Name: Redbol-Cloak

    Exports: [
        encloak2 decloak2
    ]

    Description: {
        ENCLOAK was a very insecure encryption that was included with Rebol2.
        It is unsuitable for any modern cryptographic purpose, but some things
        may have used it anyway.

        It was deleted as a native, but implemented as usermode code in case
        a script might need it for compatibility.
    }
]

cloaker: function [  ; specialized as CLOAK and DECLOAK
    {Simple and insecure data scrambler, was native C code in Rebol2/R3-Alpha}

    return: [binary!] "Same series as data"
    decode [logic?] "true if decode, false if encode"
    data [binary!] "Binary series to descramble (modified)"
    key [text! binary! integer!] "Encryption key or pass phrase"
    /with "Use a text! key as-is (do not generate hash)"
][
    if length of data = 0 [return]

    switch type of key [
        integer! [key: to binary! to string! key]  ; UTF-8 string conversion
        text! [key: to binary! key]  ; UTF-8 encoding of string
        binary! []
        fail
    ]

    klen: length of key
    if klen = 0 [
        fail "Cannot CLOAK/DECLOAK with length 0 key"
    ]

    if not with [  ; hash key (only up to first 20 bytes?)
        src: make binary! 20
        count-up i 20 [
            append src key.(1 + modulo (i - 1) klen)
        ]

        key: checksum 'sha1 src
        assert [length of key = 20]  ; size of an SHA1 hash
        klen: 20
    ]

    dlen: length of data

    ; Indexing in this routine doesn't try to get too clever; it uses the
    ; same range as the C but just indexes to `1 +` that.  Anyone who wants
    ; to "optimize" it can also worry about debugging the incompatibilities.
    ; The routines are not used anywhere relevant, AFAIK.

    if decode [
        i: dlen - 1
        while [i > 0] [
            data.(1 + i): data.(1 + i) xor+
                (data.(1 + i - 1) xor+ key.(1 + modulo i klen))
            i: i - 1
        ]
    ]

    ; Change starting byte based all other bytes.

    n: first #{A5}

    ; In the C code this just kept adding to a 32-bit number, allowing
    ; overflow...then using a C cast to a byte.  Try to approximate this by
    ; just doing the math in modulo 256
    ;
    i: 1
    while [i < dlen] [
        n: modulo (n + data.(1 + i)) 256
        i: i + 1
    ]

    data.1: me xor+ n

    if not decode [
        i: 1
        while [i < dlen] [
            data.(1 + i): data.(1 + i) xor+
                (data.(1 + i - 1) xor+ key.(1 + modulo i klen))
            i: i + 1
        ]
    ]

    return data
]

export decloak2: redescribe [
    {Decodes a binary string scrambled previously by encloak.}
](
    specialize :cloaker [decode: true]
)

export encloak2: redescribe [
    {Scrambles a binary string based on a key.}
](
    specialize :cloaker [decode: false]
)
