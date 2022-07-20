Rebol [
    Title: "Rebol2 Compatibility for UPARSE Engine"
    File: %parse2.r
    License: {LGPL 3.0}

    Type: module
    Name: Redbol-Parse

    Exports: [
        redbol-combinators
        uparse2* uparse2
    ]

    Description: {
        One of the early applications of UPARSE is to be able to implement
        backward-compatible parse behavior by means of a series of tweaks.
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
        return: "Redbol rules don't return results"
            [bad-word!]
        parser [action!]
        <local> pos
    ][
        append state.loops binding of 'return

        cycle [
            any [
                didn't [# pos]: parser input  ; failed rule => not success
                same? pos input  ; no progress => stop successfully
            ] then [
                take/last state.loops

                remainder: input
                return ~any~
            ]
            input: pos
        ]
    ]

    'some combinator [
        {(REDBOL) Must run at least one match, stop if no progress}
        return: "Redbol rules don't return results"
            [<opt> bad-word!]
        parser [action!]
        <local> pos
    ][
        append state.loops binding of 'return

        any [
            didn't [# pos]: parser input  ; failed first => stop, not success
            same? pos input  ; no progress first => stop, not success
        ] then [
            take/last state.loops
            return null
        ]
        input: pos  ; any future failings won't fail the overall rule
        cycle [
            any [
                didn't [# pos]: parser input  ; no match => stop, not success
                same? pos input  ; no progress => stop successfully
            ] then [
                take/last state.loops
                remainder: input
                return ~some~
            ]
            input: pos
        ]
    ]

    'while combinator [
        {(REDBOL) Any number of matches (including 0), no progress requirement}
        return: "Result of last successful match, or NULL if no matches"
            [<opt> any-value!]
        parser [action!]
        <local> last-result' result' pos
    ][
        append state.loops binding of 'return

        cycle [
            ([# pos]: parser input) else [
                take/last state.loops
                remainder: input  ; WHILE never fails (but REJECT?)
                return ~while~
            ]
            input: pos
        ]
        fail ~unreachable~
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
    ; Ren-C goes along with this change, including that the position is
    ; captured by `pos: <here>` instead of simply by `pos:`.  However, the
    ; concept of how combinators can produce a result to be captured is
    ; rethought.

    'copy combinator [
        {(REDBOL) Copy input series elements into a SET-WORD! or WORD!}
        return: "Redbol rules don't return results"
            [<opt> bad-word!]
        'target [word! set-word!]
        parser [action!]
    ][
        ([^ remainder]: parser input) else [
            return null
        ]
        set target copy/part input remainder
        return ~copy~
    ]

    'set combinator [
        {(REDBOL) Take single input element into a SET-WORD! or WORD!}
        return: "Redbol rules don't return results"
            [<opt> bad-word!]
        'target [word! set-word!]
        parser [action!]
        <local> pos
    ][
        ([^ remainder]: parser input) else [
            return null
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
        return: "Redbol rules don't return results"
            [<opt> bad-word!]
        value [set-word!]
    ][
        set value input
        remainder: input ; don't change position
        return ~mark~
    ]

    get-word! combinator [
        return: "Redbol rules don't return results"
            [<opt> bad-word!]
        value [get-word!]
    ][
        ; Restriction: seeks must be within the same series.
        ;
        if not same? head input head get value [
            fail "SEEK (via GET-WORD!) in UPARSE must be in the same series"
        ]
        remainder: get value
        return ~seek~
    ]

    === OLD-STYLE INTEGER! COMBINATOR ===

    ; This uses a skippable quoted integer argument to get a maximum range.
    ; It's a sketchy idea to say `1 2 rule` is different from `1 [2 rule]` in
    ; UPARSE, so this is being only done in the compatibility mode for now.

    integer! combinator [
        return: "Last parser result"
            [<opt> any-value!]
        value [integer!]
        'max [<skip> integer!]
        parser [action!]
        <local> result' last-result' temp-remainder
    ][
        all [max, max < value] then [
            fail "Can't make MAX less than MIN in range for INTEGER! combinator"
        ]

        result': @void ; `0 <any>` => void intent
        repeat value [  ; do the required matches first
            ([^result' input]: parser input) else [
                return null
            ]
        ]
        if max [  ; for "bonus" iterations, failing is okay, save last result
            last-result': result'
            repeat (max - value) [
                ([^result' temp-remainder]: parser input) else [
                    break  ; don't return null if it's in the "overage range"
                ]
                last-result': result'
                input: temp-remainder
            ]
            result': last-result'
        ]

        remainder: input
        return unmeta result'
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
        return: "Redbol rules don't return results"
            [<opt> bad-word!]
        subparser [action!]
        <local> subseries
    ][
        if tail? input [
            return null  ; `parse [] [into [some "a"]]` is false in Rebol2/Red
        ]
        if not any-series? subseries: input.1 [
            fail "Need ANY-SERIES! datatype for use with INTO in UPARSE"
        ]

        ; If the entirety of the item at the input array is matched by the
        ; supplied parser rule, then we advance past the item.
        ;
        any [
            didn't [# subseries]: subparser subseries
            not tail? subseries
        ] then [
            return null
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
        return: "Redbol rules don't return results"
            [<opt> bad-word!]
    ][
        return null
    ]
]

; Kill off any new combinators.

redbol-combinators.('between): null
redbol-combinators.('gather): null
redbol-combinators.('emit): null

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
redbol-combinators.('break): :default-combinators.('stop)
redbol-combinators.('reject): :default-combinators.('break)

; Red has COLLECT and KEEP, with different semantics--no rollback, and the
; overall parse result changes to the collect result vs. setting a variable.
; That could be emulated.
;
redbol-combinators.('collect): null
redbol-combinators.('keep): null

uparse2*: specialize :parse* [
    combinators: redbol-combinators
]
uparse2: specialize :parse*/fully [
    combinators: redbol-combinators
]
