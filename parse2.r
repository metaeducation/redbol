Rebol [
    Title: "Rebol2 Compatibility for UPARSE Engine"
    File: %parse2.r
    License: {LGPL 3.0}

    Type: module
    Name: Redbol-Parse

    Exports: [
        redbol-combinators
        uparse2
    ]

    Description: {
        One of the early applications of UPARSE is to be able to implement
        backward-compatible parse behavior by means of a series of tweaks.
    }

    Notes: {
      * Redbol combinators do not return results.  There is no attempt to
        do so in order to achieve some kind of half-baked idea of using Rebol2
        style syntax but using rule products as well; the code is kept as
        simple as possible by just giving back an isotope with the name of
        the cominator.
    }
]


redbol-combinators: copy default-combinators

append redbol-combinators spread reduce [

    === ANY AND SOME HAVE "NO PROGRESS" CONSTRAINT ===

    ; The no-progress constraint of ANY and SOME are discussed here, they are
    ; believed to perhaps make things harder to understand:
    ;
    ; https://forum.rebol.info/t/any-vs-while-and-not-end/1572/2
    ; https://forum.rebol.info/t/any-vs-many-in-parse-eof-tag-combinators/1540/10
    ;
    ; These compatibility versions are not value-bearing.

    'any combinator [
        {(REDBOL) Any number of matches (including 0), stop if no progress}
        return: [~any~]
        parser [action?]
    ][
        append state.loops binding of 'return

        remainder: input  ; if no matches, it can still succeed

        cycle [
            [^ remainder]: parser input except [
                break  ; failed rule => stop successfully
            ]
            if same? remainder input [
                break  ; no progress => stop successfully
            ]
            input: remainder  ; accept the parse progress and try again
        ]

        take/last state.loops
        return ~any~
    ]

    'some combinator [
        {(REDBOL) Must run at least one match, stop if no progress}
        return: [~some~]
        parser [action?]
        <local> no-matches
    ][
        append state.loops binding of 'return
        no-matches: true

        cycle [
            [^ remainder]: parser input except [
                break  ; failed rule => stop successfully
            ]
            if same? remainder input [
                break  ; no progress => stop successfully
            ]
            no-matches: false
            input: remainder  ; accept the parse progress and try again
        ]

        if no-matches [
            return raise "SOME did not run at least one match"
        ]

        take/last state.loops
        return ~some~
    ]

    'while combinator [
        {(REDBOL) Any number of matches (including 0), no progress requirement}
        return: [~while~]
        parser [action?]
    ][
        append state.loops binding of 'return

        cycle [
            [^ input]: parser input except [
                break
            ]
        ]

        take/last state.loops
        remainder: input  ; WHILE never fails (but REJECT?)
        return ~while~
    ]

    === OLD STYLE SET AND COPY COMBINATORS ===

    ; Historical Rebol's PARSE had SET and COPY keywords which would take
    ; a WORD! as their first argument, and then a rule.  This was done because
    ; the SET-WORD! was taken for capturing the parse position into a
    ; variable.  That idea was first overturned by a JavaScript implementation
    ; of Rebol called Topaz, using SET-WORD! for generic captures of data
    ; out of the parse input:
    ;
    ; https://github.com/giesse/red-topaz-parse
    ;
    ; Ren-C goes along with this, capturing the position with `pos: <here>`.

    'copy combinator [
        {(REDBOL) Copy input series elements into a SET-WORD! or WORD!}
        return: [~copy~]
        'target [word! set-word!]
        parser [action?]
    ][
        [^ remainder]: parser input except e -> [
            return raise e
        ]
        set target copy/part input remainder
        return ~copy~
    ]

    'set combinator [
        {(REDBOL) Take single input element into a SET-WORD! or WORD!}
        return: [~set~]
        'target [word! set-word!]
        parser [action?]
    ][
        [^ remainder]: parser input except e -> [
            return raise e
        ]
        if same? remainder input [  ; no advancement gives NONE
            set target null
        ] else [
            set target input.1  ; one unit ahead otherwise
        ]
        return ~set~
    ]

    === OLD STYLE SET-WORD! AND GET-WORD! BEHAVIOR ===

    ; This is the handling for sets and gets that are standalone...e.g. not
    ; otherwise quoted as arguments to combinators (like COPY X: SOME "A").
    ; These implement the historical behavior of saving the parse position
    ; into the variable or restoring it.

    set-word! combinator [
        return: [~mark~]
        value [set-word!]
    ][
        set value input
        remainder: input ; don't change position
        return ~mark~
    ]

    get-word! combinator [
        return: [~seek~]
        value [get-word!]
    ][
        value: get value except e -> [
            fail e  ; report error from GET (e.g. getting an unset value)
        ]

        if not any-series? value [
            fail "SEEK (via GET-WORD!) in PARSE2 must return a series"
        ]

        if not same? head input head value [
            fail "SEEK (via GET-WORD!) in PARSE2 must be in the same series"
        ]

        remainder: value
        return ~seek~
    ]

    === OLD-STYLE "NONE!" COMBINATOR (what Ren-C calls BLANK!) ===

    ; In Rebol2 and R3-Alpha, "none" is a no-op. The input type doesn't matter.
    ; In Red, "none" matches in blocks but not in strings.
    ;
    ; Ren-C matches blanks literally in array inputs, and acts like matching
    ; a space in string and binary inputs.  That's closer to Red's behavior in
    ; spirit, but here we emulate Rebol2.

    blank! combinator [
        return: [~blank~]
        value [blank!]
    ][
        remainder: input
        return ~blank~
    ]

    === OLD-STYLE INTEGER! COMBINATOR ===

    ; This uses a skippable quoted integer argument to get a maximum range.
    ; It's a sketchy idea to say `1 2 rule` is different from `1 [2 rule]` in
    ; UPARSE, so this is being only done in the compatibility mode for now.

    integer! combinator [
        return: [~integer!~]
        value [integer!]
        'max [<skip> integer!]
        parser [action?]
    ][
        all [max, max < value] then [
            fail "Can't make MAX less than MIN in range for INTEGER! combinator"
        ]

        repeat value [  ; do the required matches first
            [^ remainder]: parser input except e -> [
                return raise e
            ]
            input: remainder  ; accept the input and try again
        ]
        if max [  ; for "bonus" iterations, failing is okay
            repeat (max - value) [
                [^ remainder]: parser input except [
                    break  ; don't return failure if it's in the "overage range"
                ]
                input: remainder  ; accept the input and try again
            ]
        ]

        return ~integer!~
    ]

    === OLD-STYLE INSERT AND CHANGE (TBD) ===

    ; !!! If you are going to make a parser combinator that can distinguish
    ; between:
    ;
    ;     parse ... [insert ...]
    ;     parse ... [insert only ...]
    ;
    ; The concept of skippable quoted WORD! arguments would mean that "..."
    ; couldn't start with a WORD!, in the current framing of <skip>-pable
    ; arguments.  You'd have to write a fully variadic combinator.  Or you
    ; would make the rule that the ... had to be a GROUP! or a BLOCK!!, not
    ; just a WORD!.
    ;
    ; At time of writing, variadic combinators don't exist, and this isn't
    ; a sufficient priority to make them for.  Review later.

    === OLD-STYLE INTO BEHAVIOR ===

    ; New SUBPARSE is arity-2
    ;
    ; https://forum.rebol.info/t/new-more-powerful-arity-2-into-in-uparse/1555

    'into combinator [
        {(REDBOL) Arity-1 Form of Recursion with a rule}
        return: [~into~]
        subparser [action?]
        <local> subseries
    ][
        if tail? input [
            return raise "INTO used at end of PARSE input"
        ]
        if not any-series? subseries: input.1 [
            fail "Need ANY-SERIES! datatype for use with INTO in UPARSE"
        ]

        ; If the entirety of the item at the input array is matched by the
        ; supplied parser rule, then we advance past the item.
        ;
        [@ subseries]: subparser subseries except e -> [return raise e]

        if not tail? subseries [
            return raise "INTO rule did not consume entirety of subseries"
        ]

        remainder: next input
        return ~into~
    ]

    === OLD-STYLE AND SYNONYM FOR AHEAD ===

    ; AND is a confusing name for AHEAD, doesn't seem to be a lot of value
    ; in carrying that synonym forward in UPARSE.
    ;
    'and :default-combinators.('ahead)

    === END AS WORD INSTEAD OF TAG ===

    'end :default-combinators.(<end>)

    === OLD-STYLE FAIL INSTRUCTION ===

    ; In Ren-C, the FAIL word is taken to generally relate to raising errors.
    ; PARSE was using it to mean a forced mismatch.
    ;
    ; Ren-C rethinks this so that logic #[false] is used to indicate a match
    ; has failed, and to roll over to the next alternate (if any).  By making
    ; the logic #[true] mean "keep parsing", this allows evaluated expressions
    ; that are substituted into the parse stream via GET-GROUP! to control
    ; whether parsing continues or not.

    'fail combinator [
        {(REDBOL) Interrupt matching with failure}
        return: []  ; divergent
    ][
        return raise "Explicit FAIL combinator usage in PARSE"
    ]
]

; Rename ACCEPT to RETURN (UPARSE renamed it)

redbol-combinators.('return): default-combinators.('accept)
redbol-combinators.('accept): void

; Rename TRY to OPT

redbol-combinators.('opt): default-combinators.('try)
redbol-combinators.('try): void

; Kill off any new combinators.

redbol-combinators.('between): void
redbol-combinators.('gather): void
redbol-combinators.('emit): void

; Ren-C rethought BREAK to mean soft failure, e.g. the looping construct of
; SOME or REPEAT will be NULL.  The precise meaning and behavior of historical
; BREAK and REJECT is not very coherent:
;
;     red>> parse "aaa" [opt some ["a" reject] "aaa"]
;     == false
;
;     red>> parse "aaa" [some ["a" reject] | "aaa"]
;     == true
;
redbol-combinators.('break): default-combinators.('stop)
redbol-combinators.('reject): default-combinators.('break)

; Red has COLLECT and KEEP, with different semantics--no rollback, and the
; overall parse result changes to the collect result vs. setting a variable.
; That could be emulated.
;
redbol-combinators.('collect): void
redbol-combinators.('keep): void

uparse2: specialize :parse [
    combinators: redbol-combinators
]
