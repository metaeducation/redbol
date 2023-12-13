Rebol [
    Title: "Emulation of Rebol2 IO"
    File: %io2.r
    License: {LGPL 3.0}

    Type: module
    Name: Redbol-IO

    Exports: [
        print2 split-path-2 write2 read2 load2
    ]

    Description: {
        File I/O and related functions.
    }
]

print2: func [
    return: [~]
    value [any-value!]  ; Ren-C only takes TEXT!, BLOCK!, BLANK!, CHAR?
][
    write-stdout case [
        block? :value [spaced value]
    ] else [
        form :value
    ]
    write-stdout newline
]

split-path-2: func [  ; Ren-C version uses multi-return
    return: [block!]
    target [file! url!]
    <local> dir pos text
][
    text: as text! target
    pos: _
    parse text [
        ["/" | "." try "." try "/"] <end> (dir: dirize text) |
        pos: <here>, try some [thru "/" [<end> | pos: <here>]] (
            all [
                empty? dir: copy/part text (at head of text index of pos),
                dir: %./
            ]
            all [find [%. %..] pos: to file! pos insert tail of pos "/"]
        )
        <end>
    ]
    return reduce [(as type of target dir) pos]
]

write2: adapt (augment :write [
    /binary "Preserves contents exactly."
    /direct "Opens the port without buffering."
    /no-wait "Returns immediately without waiting if no data."
    /with "Specifies alternate line termination."
        [char? text!]
    /allow "Specifies the protection attributes when created."
        [block!]  ; this is still on WRITE, but not implemented (?)
    /mode "Block of above refinements."
        [block!]
    /custom "Allows special refinements."
        [block!]
    /as {(Red) Write with the specified encoding, default is 'UTF-8}
        [word!]
]) [
    all [binary? data, not binary] then [
        fail [
            {Rebol2 would do LF => CR LF substitution in BINARY! WRITE}
            {unless you specified /BINARY.  Doing this quietly is a bad}
            {behavior.  Use /BINARY, or WRITE AS TEXT! for conversion.}
        ]
    ]

    for-each w [direct no-wait with part allow mode custom as] [
        if get w [
            fail [unspaced ["write/" w] "not currently in Redbol"]
        ]
    ]
]

read2: enclose (augment :read [
    /binary "Preserves contents exactly."
    /direct "Opens the port without buffering."
    /no-wait "Returns immediately without waiting if no data."
    /with "Specifies alternate line termination."
        [char? text!]
    /mode "Block of above refinements."
        [block!]
    /custom "Allows special refinements."
        [block!]
    /as {(Red) Read with the specified encoding, default is 'UTF-8}
        [word!]
]) func [f [frame!]] [
    for-each w [direct no-wait with part mode custom as] [
        if f.(w) [
            fail [unspaced ["read/" w] "not currently in Redbol"]
        ]
    ]

    ; !!! Rebol2 defaulted READ to be TEXT!.  Is Red preserving this?
    ;
    return if f.binary [do f] else [as text! do f]
]

; Ren-C's LOAD uses "ALL" semantics by default to give back a BLOCK! of code
; always.  Extracting single values is done with LOAD-VALUE.
;
; Historical Redbol is more unpredictable in the name of "convenience":
;
;    rebol2> load "1"
;    == 1
;
;    rebol2> load "1 2"
;    == [1 2]
;
; This augments LOAD with the /ALL refinement and tweaks the behavior.
;
load2: enclose (augment :load [/all]) func [f <local> try-one-item] [
    try-one-item: not f.all
    result: do f  ; now always BLOCK! if LOADing Rebol code

    if try-one-item and (block? result) and (length of result = 1) [
        return first result  ; "1" loads as `[1]`, change it to `1`
    ]
    return result  ; "1 2" loads as `[1 2]`, leave it that way
]
