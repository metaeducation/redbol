REBOL [
    System: "Rebol 3 (Ren-C Branch)"
    Title: "Rebol2 and Red Compatibility Shim"
    Homepage: https://trello.com/b/l385BE7a/porting-guide
    Rights: {
        Copyright 2012-2022 Ren-C Open Source Contributors
        REBOL is a trademark of REBOL Technologies
    }
    Type: module
    Name: Redbol
    License: {
        Licensed under the Lesser GPL, Version 3.0 (the "License");
        you may not use this file except in compliance with the License.
        You may obtain a copy of the License at

        https://www.gnu.org/licenses/lgpl-3.0.html
    }
    Description: {
        This module attempts to adapt Ren-C so that basic functionality will
        respond similarly to the compatible subset of Rebol2 and Red.

        The current lack of a GUI in Ren-C means that this will only be
        useful for command-line scripts and utilities.  However, it serves as
        a test of the system's flexibility, as well as a kind of "living
        documentation" of the nuances of what has been changed.
    }
    Notes: {
        * Ren-C does not allow the mutation of PATH!.  You can JOIN a path to
          make a new one, and FOREACH a path to enumerate one, but you can't
          APPEND or INSERT into them.  Calling code that expects to do these
          kinds of mutations needs to be changed to do them on BLOCK! and
          convert to PATH! when done.
    }
]


export ren: lib  ; save the Ren-C library
export lib: does [print "Use REN or REDBOL in Redbol implementation, not LIB"]
redbol: attach of 'redbol  ; not exported--local for disambiguating this file


=== WRAPPER FOR "EMULATION-DEFINITIONS-ARE-IN-PROGRESS" ===

; This module is redefining the workings of the system fundamentally.  While
; doing those definitions it's preferable to not have to say `ren.switch`
; or otherwise prefix each call in the implementation so it doesn't use the
; new definitions that have been made so far.
;
; Until a module feature to facilitate this kind of thing becomes standard,
; this binds the bodies of EMULATE or HELPER things into lib for you.


helper: ren.lambda [
    {NON-EXPORTED definition relying on words in REN (e.g. baseline APPEND)}
    code [block!]
] ren.in ren [
    do in ren code
]

emulate: enfix ren.lambda [
    {EXPORTED definition that relying on words in REN (e.g. baseline APPEND)}
    :set-word [set-word!]
    code [block!]
    <local> temp
] ren.in ren [
    export (set-word): do in ren code
]


=== DATATYPES ===

string!: emulate [text!]
string?: emulate [:text?]
to-string: emulate [specialize :to [type: text!]]

; There is no CHAR! "datatype" in Ren-C (ISSUE! and char are unified)
; The belief is that TO a char-like thing of 1 should be #"1"
; Currently AS is serving as the numeric converter.
;
to-char: emulate [
    lambda [value] [
        either integer? value [
            as issue! value
        ][
            to issue! value
        ]
    ]
]

paren!: emulate [group!]
paren?: emulate [:group?]
to-paren: emulate [specialize :to [type: group!]]

number!: emulate [any-number!]
number?: emulate [:any-number?]
scalar!: emulate [any-scalar!]
scalar?: emulate [:any-scalar?]
series!: emulate [any-series!]
series?: emulate [:any-series?]

any-type!: emulate [any-value!]  ; !!! does not include any "UNSET!"

any-block!: emulate [any-array!]
any-block?: emulate [:any-array?]

any-object!: emulate [any-context!]
any-object?: emulate [:any-context?]

; Redbol wants something reified that does not reduce that is falsey.
;
; Ren-C has no such concept in reified space.  We use the-word! for now because
; it's a datatype that didn't exist in historical Rebol, so it should not
; conflate with issues/etc...and it is non-reducing.  Functions like IF and
; CASE have to be hooked with new predicates for the revised definition of
; truthiness and falseyness that includes @none
;
none: emulate [@none]
none!: emulate [the-word!]
none?: emulate [x -> [:x = @none]]

type?: emulate [
    lambda [
        value [<opt> any-value!]
        /word {Note: SWITCH evaluates https://trello.com/c/fjJb3eR2}
    ][
        case [
            not word [either @none = :value [none!] [type of :value]]
            unset? 'value ['unset!]  ; https://trello.com/c/rmsTJueg
            :value = @none ['none!]  ; https://trello.com/c/vJTaG3w5
            group? :value ['paren!]  ; https://trello.com/c/ANlT44nH
            (match [lit-word?] :value) ['lit-word!]
            (match [lit-path?] :value) ['lit-path!]
        ] else [
            to-word type of :value
        ]
    ]
]

true?: emulate [x -> [not any [not :x, :x = @none]]]
false?: emulate [x -> [did any [not :x, :x = @none]]]
export not: :false?

=== FUNCTIONS ===

import <function2.r>

any-function!: emulate [action?!]
any-function?: emulate [:action?]

function!: emulate [action?!]
function?: emulate [:action?]

native!: emulate [action?!]
native?: emulate [:action?]

closure!: emulate [:action?!]
closure?: emulate [:action?]

; If a Ren-C function suspects it is running code that may happen more than
; once (e.g. a loop or function body) it marks that parameter `<const>`.
; That prevents casual mutations.
;
; !!! This depended on the RESKINNED function, which was removed to make way
; for more coherent granular parameter tweaking in the spec and function
; creation with AS FRAME!.  In order to begin implementing that approach
; correctly, the bad implementation of RESKINNED had to be pulled out.  For
; the moment, the const parameter is not tweaked in Redbol...but the feature
; is aiming to come back shortly in much better form.

func: emulate [:func2]
function: emulate [:function2]
apply: emulate [:apply2]
has: emulate [:has2]
does: emulate [:does2]

; Some of CLOSURE's functionality was subsumed into all FUNCTIONs, but
; the indefinite lifetime of all locals and arguments was not.
; https://forum.rebol.info/t/234
;
closure: emulate [:function2]
clos: emulate [:func2]




for-each-nonconst: emulate [
;    reskinned [
;        body [block!]  ; no <const> annotation
;    ] adapt :for-each []  ; see RESKINNED for why this is an ADAPT for now

    :for-each
]

?: emulate [:help]

to-local-file: emulate [
    if undefined? 'file-to-local [
        does [fail "TO-LOCAL-FILE not available in web build"]
    ] else [
        get 'file-to-local
    ]
]

to-rebol-file: emulate [
    if undefined? 'local-to-file [
        does [fail "LOCAL-TO-FILE not available in web build"]
    ] else [
        get 'local-to-file
    ]
]

why?: emulate [does [ren.why]]  ; not exported yet, :why not bound

null: emulate [
    make issue! 0  ; NUL in Ren-C https://en.wikipedia.org/wiki/Null_character
]

comment: emulate [
    func [
        return: [~] {Not invisible: https://trello.com/c/dWQnsspG}
        :discarded [block! any-string! binary! any-scalar!]
    ][
    ]
]

found?: emulate [
    func [
        {See DID and NOT: https://trello.com/c/Cz0qs5d7}
        return: [logic?]
        value
    ][
        return not blank? :value
    ]
]


=== SETTING AND GETTING ===

unset!: emulate [antiform!]
unset?: emulate [:unset?]  ; checks *value* is unset, not var
unset: emulate [:unset]

; Note: R3-Alpha had a /PAD option, which was the inverse of /SOME.
; If someone needs it, they can adapt this routine as needed.
;
set: emulate [
    func [
        return: [<opt> any-value!]
        target [any-word! any-path! block! object!]
        value [<opt> any-value!]
        /any "Allow UNSET as a value rather than causing an error"
        /only "Block or object value argument is set as a single value"
        /some "None values in a block or object value argument, are not set"
    ][
        let set_ANY: any
        any: :lib.any

        all [  ; !!! is it necessary to impose this historical restriction?
            not set_ANY
            unset? 'value
            fail "Can't SET a value to UNSET! unless SET/ANY is used"
        ]

        if not block? target [  ; handle simple WORD!/PATH! case
            return set target :value
        ]

        if object? target [  ; turn OBJECT! case into BLOCK! case
            target: words of target
        ]

        if only or (not block? :value) [  ; don't set itemwise, all get same
            for-each t target [set t :value]
            return :value
        ]

        let block: value  ; save so we can return at same position
        for-each t target [
            if blank? try :block.1 [  ; may be at end of block, block.1 = null
                if not some [set t blank]
            ] else [
                set t :block.1
            ]
            block: try next block
        ]
        return value
    ]
]

get: emulate [
    func [
        {Now no OBJECT! support, unset vars always null}
        return: [<opt> any-value!]
        source {Legacy handles Rebol2 types, not *any* type like R3-Alpha}
            [blank! any-word! any-path! any-context! block!]
        /any
    ][
        let any_GET: any
        any: :lib.any

        if block? :source [
            return source  ; this is what it did :-/
        ]
        if any-context? source [
            return apply :get [words of source /any any_GET]
        ]
        return apply :get [source /any any_GET]
    ]
]

value?: emulate [
    func [
        {See SET? in Ren-C: https://trello.com/c/BlktEl2M}
        return: [logic?]
        value
    ][
        return either any-word? :value [set? value] [true]  ; bizarre.  :-/
    ]
]


; R3-Alpha and Rebol2's DO was effectively variadic.  If you gave it an
; action, it could "reach out" to grab arguments from after the call.  Ren-C
; replaced this functionality with EVAL:
;
; https://forum.rebol.info/t/meet-the-eval-native/311
;
; !!! This code contains an early and awkward attempt at emulating the old
; DO behavior for functions in userspace, through an early version of
; variadics.  Ren-C is aiming to have functions that make "writing your own
; EVAL-like-thing" easier.
;
do: emulate [
    func [
        return: [<opt> any-value!]
        source [<opt> blank! block! group! text! binary! url! file! tag!
            error! action?
        ]
        normals [any-value! <variadic>]
        'softs [any-value! <variadic>]
        :hards [any-value! <variadic>]
        /args [any-value!]
        /next [word!]
    ][
        let var: next
        next: :lib.next

        if var [  ; DO/NEXT
            if args [fail "Can't use DO/NEXT with ARGS"]
            let [result 'source]: evaluate :source
            set var source  ; DO/NEXT put the *position* in the var
            return :result  ; DO/NEXT returned the *evaluative result*
        ]

        if action? :source [
            code: reduce [:source]
            params: parameters of :source
            iterate params [
                append code switch/type params.1 [
                    word! [take normals]
                    lit-word?! [take softs]
                    get-word! [take hards]
                    set-word! [[]]  ; empty block appends nothing
                    refinement! [break]

                    fail ["bad param type" params.1]
                ]
            ]
            return do code
        ]
        return do/args :source :args  ; if args is null, refinement is "revoked"
    ]
]

to: emulate [
    enclose :to func [f] [
        all [
            :f.value = group!
            find any-word! f.type
            return as type! 'paren!
        ]
        all [
            f.type = char!
            integer? :f.value
            return make issue! :f.value
        ]
        all [
            find any-array! f.type
            binary? :f.value
            return as f.type transcode f.value
        ]
        return do f
    ]
]

try: emulate [
    lambda [
        {See TRAP for Ren-C equivalent: https://trello.com/c/IbnfBaLI}
        block [block!]
        /except "Note TRAP doesn't take a handler...use THEN instead"
            [<unrun> block! frame!]
        <local>
            error result
    ][
        if ([error result]: trap [do block else '_]) [
            case [
                not except [error]
                block? except [do except else '_]
                frame? except [apply except [error] else '_]
            ]
        ] else [
            result else '_  ; Note: may be an ERROR! that was evaluated to
        ]
    ]
]

default: emulate [
    lambda [
        {See the new enfix DEFAULT: https://trello.com/c/cTCwc5vX}
        'word [word! set-word! lit-word?]
        value
    ][
        any [
            unset? word
            blank? get word
        ] then [
            set word :value
        ] else [
            :value
        ]
    ]
]

also: emulate [
    lambda [
        {Supplanted by ELIDE: https://trello.com/c/pGhk9EbV}
        returned [<opt> any-value!]
        discarded [<opt> any-value!]
    ][
        :returned
    ]
]


=== PARSE ===

import <parse2.r>

; PARSE in Ren-C is vastly redesigned, but the goal is that it act as a
; framework (codename "UPARSE") that can be easily twistable for compatibility:
;
; https://forum.rebol.info/t/introducing-uparse-the-hackable-usermode-parse/1529
;
; UPARSE is in early development at time of writing and is very slow.  But the
; goal is to speed it up over time.  But Redbol uses it today anyway.

parse: emulate [
    func [
        {Non-block rules replaced by SPLIT: https://trello.com/c/EiA56IMR}
        return: [logic? block!]
        input [any-series!]
        rules [block! text! blank!]
        /case
        /all "Ignored refinement in <r3-legacy>"
    ][
        let case_PARSE: case
        case: :lib.case

        comment [all_PARSE: all]  ; Not used
        all: :lib.all

        return switch type of rules [
            blank! [split input charset reduce [tab space CR LF]]
            text! [split input to-bitset rules]
        ] else [
            did apply :uparse2 [input rules /case case_PARSE]
        ]
    ]
]


=== EVALUATING ===

reduce: emulate [
    lambda [
        value "Not just BLOCK!s evaluated: https://trello.com/c/evTPswH3"
        /into "https://forum.rebol.info/t/stopping-the-into-virus/705"
            [any-array!]
    ][
        case [
            not block? :value [:value]
            into [insert into reduce :value]
        ] else [
            reduce :value
        ]
    ]
]

compose: emulate [
    lambda [
        value "Ren-C does not splice by default (needs SPREAD)" [any-value!]
        /deep "Ren-C recurses into PATH!s: https://trello.com/c/8WMgdtMp"
        /only
        /into "https://forum.rebol.info/t/stopping-the-into-virus/705"
            [any-array! any-string! binary!]
    ][
        if not block? value [return value]  ; `compose 1` is `1` in Rebol2

        let composed: apply :compose [
            ;
            ; !!! Note: COMPOSE has a LABEL argument that is <skip>-able.
            ; Skippable arguments are entwined with quoting and detection, and
            ; as such have more in common with refinements than ordinary
            ; arguments.  If you want a skippable argument in an APPLY you
            ; must specify it explicitly by name...APPLY always <skip>s.

            value
            /deep deep

            ; The predicate is a function that runs on whatever is generated
            ; in the COMPOSE'd slot.  If you put it in a block, that will
            ; splice but protect its contents from splicing (the default).
            ; We add the twist that `~` antiforms subvert errors in Rebol2.
            ;
            ;    rebol2> type? either true [] []
            ;    == unset!
            ;
            ;    rebol2> compose [(either true [] [])]
            ;    == []
            ;
            /predicate if not only [
                lambda [group <local> product] [
                    product: eval group else [@none]
                    (non any-array! :product) else array -> [spread array]
                ]
            ]
        ]

        either into [insert into composed] [composed]
    ]
]

collect: emulate [
    lambda [
        body [block!]
        /into "https://forum.rebol.info/t/stopping-the-into-virus/705"
            [any-series!]
    ][
        let out: any [into, make block! 16]

        let keeper: specialize* (
            enclose* :insert func* [
                f [frame!]
                <with> out
            ][
                f.series: out  ; want new series position capture each time
                :f.value  ; evalutate input before the DO to be return result
                elide out: do f  ; update position on each insertion

                ; original f.value will be returned due to ELIDE
            ]
        )[
            series: <remove-unused-series-parameter>
        ]

        reeval func* compose [(name) [action?] <with> return] body :keeper
        either into [out] [head of out]
    ]
]

repend: emulate [
    lambda [
        series [any-series! port! map! object! bitset!]
        value
        /part [any-number! any-series! pair!]
        /only
        /dup [any-number! pair!]
    ][
        apply :redbol.append [  ; Want overridden APPEND semantics (vs Ren-C)
            series
            either block? value [reduce value] [value]
            /part part
            /only only
            /dup dup
        ]
    ]
]


; REJOIN in R3-Alpha meant "reduce and join" and was arity-1.  It was used
; in many places, such as producing strings out of blocks of string parts and
; expressions.  But it also had some really wonky properties:
;
; https://forum.rebol.info/t/rejoin-ugliness-and-the-usefulness-of-tests/248/
;
; Ren-C eliminates it, and pushes on the definition of JOIN as arity-2...where
; if there was a REJOIN operation, it would be JOIN/REDUCE.  But there is no
; arity-1 REJOIN parallel in Ren-C at time of writing...you can ask JOIN to
; have its first argument as a datatype! and it will produce that type, which
; substitutes for the intent.
;
rejoin: emulate [
    func [
        {Reduces and joins a block of values}

        return: "Same type as first non-null item produced by evaluation"
            [issue! any-series! any-sequence!]
        block "Values to reduce and join together"
            [block!]
        <local> base
    ][
        cycle [  ; Keep evaluating until a usable BASE is found

            if not (base: evaluate/next block 'block, block) [
                return copy []  ; exhausted block without finding a base value
            ]

            any [
                null? :base  ; consider to have dissolved
                blank? :base  ; treat same as NULL
            ] then [
                continue  ; do another evaluation step
            ]

            ; !!! Historical Rebol would default to a TEXT! if the first thing
            ; found wasn't JOIN-able.  This is questionable.
            ;
            if not match [issue! any-sequence! any-series!] :base [
                base: to text! :base
            ]

            return join base spread reduce block  ; JOIN what's left of block
        ]
    ]
]

join: emulate [
    lambda [value rest] [
        print "WE ARE THE CHAMPIONS!"
        apply :append [
            if series? value [copy value] else [form value]
            if block? rest [spread reduce rest] else [rest]
        ]
    ]
]

ajoin: emulate [:unspaced]

reform: emulate [:spaced]

form: emulate [
    lambda [
        value [<opt> any-value!]
        /unspaced "Outer level, append {} [1 2 [3 4]] => {123 4}"
    ][
        case [
            issue? :value [
                as text! value  ; e.g. Rebol2 said `form #<<` was `<<`
            ]
            word? :value [
                as text! value
            ]
            decimal? :value [
                ;
                ; Regarding IEEE `double` values, Wikipedia says:
                ;
                ;    "The 53-bit significand precision gives from 15 to 17
                ;     significant decimal digits precision"
                ;
                ; Rebol2 printed 15 digits after the decimal point.  R3-Alpha gave
                ; 16 digits...as does Red and seemingly JavaScript.
                ;
                ;     rebol2>> 1 / 3
                ;     == 0.333333333333333
                ;
                ;     r3-alpha>> 1 / 3
                ;     == 0.3333333333333333
                ;
                ;     red>> 1 / 3
                ;     == 0.3333333333333333
                ;
                ;     JavaScript> 1 / 3
                ;     -> 0.3333333333333333  ; Chrome
                ;     -> 0.3333333333333333  ; Firefox
                ;
                ; While this may seem a minor issue, generated output in diff
                ; gets thrown off, making it hard to see what has changed.
                ; It can't be addressed via rounding, because rounding
                ; floating point numbers can't guarantee a digit count when
                ; printing--since some numbers aren't evenly representible.
                ;
                ; This truncates the number to the right length but doesn't
                ; round it.  That would be more complicated, and is probably
                ; best done via C code once Redbol is an extension.
                ;
                value: form value
                if not find value "E" [
                    use [pos] [
                        all [
                            pos: try skip (find value ".") 15
                            clear pos
                        ]
                    ]
                ]
                value
            ]
            block? value [
                delimit: either unspaced [:lib.unspaced] [:lib.spaced]
                delimit map-each item value [
                    redbol.form :item
                ]
            ]
        ] else [
            form value
        ]
    ]
]

quit: emulate [
    lambda [
        /return "Ren-C is variadic, 0 or 1 arg: https://trello.com/c/3hCNux3z"
            [<opt> any-value!]
    ][
        apply :quit [/with :return]
    ]
]


; OBJECT is a noun-ish word; Ren-C tried HAS for a while and did not like it.
; A more generalized version of CONSTRUCT is being considered:
;
; https://forum.rebol.info/t/has-hasnt-worked-rethink-construct/1058
;
object: emulate [
    specialize :make [type: object!]
]

construct: emulate [
    lambda [
        spec [block!]
        /with [object!]
        /only
    ][
        if only [
            fail [
                {/ONLY not yet supported in emulation layer for CONSTRUCT}
                {see %redbol.reb if you're interested in adding support}
            ]
        ]
        to any [with object!] spec
    ]
]

break: emulate [
    lambda [
        /return "/RETURN is deprecated: https://trello.com/c/cOgdiOAD"
            [any-value!]
    ][
        if return [
            fail [
                "BREAK/RETURN not implemented in Redbol emulation, use THROW"
                "and CATCH.  See https://trello.com/c/uPiz2jLL/"
            ]
        ]
        break
    ]
]

++: emulate [
    func [] [
        fail @return [
            {++ and -- are not in the Redbol layer by default, as they were}
            {not terribly popular to begin with...but also because `--` is}
            {a very useful and easy-to-type dumping construct in Ren-C, that}
            {comes in very handy when debugging Redbol.  Implementations of}
            {++ and -- are available in %redbol.reb if you need them.}
            {See also ME and MY: https://trello.com/c/8Bmwvwya}
        ]
    ]
]

comment [  ; ^-- see remark above
    ++: emulate [
        lambda [
            {Deprecated, use ME and MY: https://trello.com/c/8Bmwvwya}
            'word [word!]
        ][
            value: get word  ; returned value
            elide (set word case [
                any-series? :value [next value]
                integer? :value [value + 1]
            ] else [
                fail "++ only works on ANY-SERIES! or INTEGER!"
            ])
        ]
    ]

    --: emulate [
        lambda [
            {Deprecated, use ME and MY: https://trello.com/c/8Bmwvwya}
            'word [word!]
        ][
            value: get word  ; returned value
            elide (set word case [
                any-series? :value [next value]
                integer? :value [value + 1]
            ] else [
                fail "-- only works on ANY-SERIES! or INTEGER!"
            ])
        ]
    ]
]

compress: emulate [
    function [
        {Deprecated, use DEFLATE or GZIP: https://trello.com/c/Bl6Znz0T}
        return: [binary!]
        data [binary! text!]
        /part [any-value!]
        /gzip
        /only
    ][
        any [gzip, only] else [  ; assume caller wants "Rebol compression"
            data: to-binary copy/part data part
            zlib: zdeflate data

            length-32bit: modulo (length of data) (to-integer power 2 32)
            repeat 4 [
                append zlib modulo (to-integer length-32bit) 256
                length-32bit: me / 256
            ]
            return zlib  ; ^-- plus size mod 2^32 in big endian
        ]

        return deflate/part/envelope data :lim [
            gzip [assert [not only] 'gzip]
            not only ['zlib]
        ]
    ]
]

decompress: emulate [
    func [
        {Deprecated, use DEFLATE or GUNZIP: https://trello.com/c/Bl6Znz0T}
        return: [binary!]
        data [binary!] "Red assumes GZIP, Rebol assumed 'Rebol compressed'"
        /part [binary!] "R3-Alpha refinement, must match end of compression"
        /gzip "R3-Alpha refinement (no size argument, envelope stores)"
        /limit [integer!] "R3-Alpha refinement, error if larger"
        /zlib [integer!] "Red refinement (RFC 1951), uncompressed size"
        /deflate [integer!] "Red refinement (RFC 1950), uncompressed size"
    ][
        any [gzip, zlib, deflate] else [
            ;
            ; Assume data is "Rebol compressed".  Could get more compatibility
            ; by testing for gzip header or otherwise having a fallback, as
            ; Red went with a Gzip default.
            ;
            part: default [tail of data]
            return zinflate/part/max data (skip part -4) limit
        ]

        return inflate/part/max/envelope data part limit case [
            gzip [assert [not zlib not deflate] 'gzip]
            zlib [assert [not deflate] 'zlib]
            deflate [_]
            fail
        ]
    ]
]

and: emulate [enfix :intersect]
or: emulate [enfix :union]
xor: emulate [enfix :difference]

mod: emulate [:modulo]  ; MOD is enfix in Ren-C, MODULO still prefix

; Ren-C NULL means no branch ran, Rebol2 this is communicated by #[none].  We
; use the inert WORD! form @none for this.
;
denuller: helper [
    action -> [
        chain [
            :action

            lambda [^x [<opt> <void> pack? any-value!]] [
                (unmeta x) else [@none]
            ]
        ]
    ]
]

=== CONDITIONALS ===

; Concept of "truthy" and "falsey" are different in Ren-C, where NULL is falsey
; and all ANY-VALUE! are true.  We want to make exceptions for reified ideas
; of LOGIC? and NONE? for Redbol.  Currently the only way to do that is to
; leverage the predicates of the conditionals.

if: emulate [denuller adapt :if [condition: true? :condition]]
either: emulate [denuller adapt :either [condition: true? :condition]]
unless: emulate [denuller adapt :if [condition: false? :condition]]

case: emulate [denuller specialize :case [predicate: :true?]]
any: emulate [denuller specialize :all [predicate: :true?]]
all: emulate [denuller adapt :any [predicate: :true?]]


switch: emulate [  ; Ren-C evaluates cases: https://trello.com/c/9ChhSWC4/
    enclose (augment :switch [
        /default "Default case if no others are found"
            [block!]
    ]) lambda [f [frame!]] [
        f.cases: map-each c f.cases [
            match block! c else [quote c]  ; suppress eval on non-blocks
        ]
        let def: f.default  ; the DO expires frame right now (for safety)
        (do f else (def)) else [@none]
    ]
]

for: emulate [denuller :cfor]

while: emulate [denuller :while]
foreach: emulate [
    func [
        {No SET-WORD! capture, see https://trello.com/c/AXkiWE5Z}
        return: [<opt> any-value!]
        'vars [word! block!]
        data [any-series! any-context! map! blank!]
        body [block!]
    ][
        any [
            not block? vars
            for-each-nonconst item vars [if set-word? item [break] true]
        ] then [
            return (for-each-nonconst :vars data body else [@none])
        ]

        ; Weird FOREACH, transform to WHILE: https://trello.com/c/AXkiWE5Z
        ;
        use :vars [
            let position: data
            return while [not tail? position] compose [
                (spread collect [
                    for-each item vars [
                        case [
                            set-word? item [
                                keep compose [(item) position]
                            ]
                            word? item [
                                keep compose [
                                    (to-set-word :item) position.1
                                    position: next position
                                ]
                            ]
                            fail "non SET-WORD?/WORD? in FOREACH vars"
                        ]
                    ]
                ])
                (as group! body)
            ]
        ]
    ]
]

loop: emulate [denuller :repeat]

; REPEAT in Rebol2 with an ANY-SERIES! argument acted like a FOR-EACH on that
; series.  This is redundant with FOR-EACH.
;
; R3-Alpha changed the semantics to be like a FOR-NEXT (e.g. FORALL) where you
; could specify the loop variable instead of insisting your loop variable be
; the data you are iterating.
;
; Red forbids ANY-SERIES! as the argument of what to iterate over.
;
; https://trello.com/c/CjEfA0ef
;
; The common denominator here is to act like COUNT-UP.
;
repeat: emulate [denuller :count-up]

forall: emulate [denuller :iterate]
forskip: emulate [denuller :iterate-skip]


; !!! This used to be in %mezz-legacy.r where it was being tested.  Now the
; core functions do not have /ONLY at all and the refinement only exists in
; the Redbol versions of APPEND/INSERT/CHANGE/FIND.  Would be faster to fold
; this in together with the splicing adjustment...though having it built out
; of a more convoluted composition breaks the parts up better and exercises
; more situations.
;
onlify: helper [
    func [
        {Add /ONLY behavior to APPEND, INSERT, CHANGE, FIND, SELECT...}
        return: [action?]
        action [<unrun> frame!]
        /param [word!]
    ][
        param: default ['value]
        return adapt (
            augment action [/only]
        ) compose/deep [
            all [not only, any-array? series, any-array? (param)] then [
                set/any '(param) spread (param)
            ]
            ; ...fall through to normal handling
        ]
    ]
]

; 1. Ren-C (and Red) do not support FIND on object, which returns a LOGIC! in
;    Rebol2 based on if the key is present.
;
; 2. Historically Rebol/Red consider `find "abc" ""` to be NONE.  While it
;    could be argued what this "should" be, the fact that BLANK! can be used
;    to opt out of the search suggests the opportunity is being lost to have
;    a way of opting into a match unconditionally.
;
; Also--Red changed FIND/MATCH to not imply /TAIL.  This was something we'd
; wanted in Ren-C so it was changed as well.  But this means Red and Rebol2
; will differ on that point.
;
find: emulate [
    enclose (onlify/param :find 'pattern) func [f] [
        if object? f.pattern [  ; see [1]
            return did in series pattern
        ]
        all [
            any-series? f.pattern
            empty? f.pattern  ; see [2]
        ] then [return @none]

        return (do f else [@none])
    ]
]
select: emulate [denuller :select]
pick: emulate [denuller :pick]

first: emulate [denuller :first]
first+: emulate [
    enclose :first lambda [f] [
        use [loc] [
            loc: f.location
            do f
            elide take loc
        ]
    ]
]
second: emulate [denuller :second]
third: emulate [denuller :third]
fourth: emulate [denuller :fourth]
fifth: emulate [denuller :fifth]
sixth: emulate [denuller :sixth]
seventh: emulate [denuller :seventh]
eighth: emulate [denuller :eighth]
ninth: emulate [denuller :ninth]
tenth: emulate [denuller :tenth]

query: emulate [denuller :query]
wait: emulate [denuller :wait]
bind?: emulate [denuller specialize :of [property: 'binding]]
bound?: emulate [denuller specialize :of [property: 'binding]]


; Non-strict equality does not consider quoting in historical Redbol.  This
; is changed in Ren-C:
;
; https://forum.rebol.info/t/1133/7

noquoter: helper [
    lambda [f] [adapt :f [value1: my noquote, value2: my noquote]]
]

equal?: emulate [noquoter :equal?]
not-equal?: emulate [noquoter :not-equal?]
=: emulate [enfix noquoter :=]
<>: emulate [enfix noquoter :<>]
!=: emulate [enfix noquoter :!=]


; https://forum.rebol.info/t/justifiable-asymmetry-to-on-block/751
;
oldsplicer: helper [
    lambda [action [<unrun> frame!]] [
        adapt action [
            all [
                not only, any-array? series,
                quoted? value, any-path? value
            ] then [
                set/any 'value spread as block! value  ; splice it
            ] else [
                (match [map! object!] series) then [
                    set/any 'value spread ensure block! value
                ]
            ]

            ; Red and R3-Alpha would allow you to append an INTEGER! to a
            ; BINARY! and treat it as a byte.  But Rebol2 would add it as the
            ; character of the digit.  We go ahead and add as a byte because
            ; there's no good way to add a byte otherwise.  (char encodings no
            ; longer guarantee going to a single byte.)
            ;
            ; But if the integer is in a block, we fall back to Rebol2 behavior
            ;
            ;     rebol2> append bin [1234]
            ;     == #{32353731323334}
            ;
            ;     r3-alpha/red> append bin [1234]
            ;     *** Script Error: value out of range: 1234
            ;
            ; It would also spell WORD!s as their Latin1 values.
            ;
            all [
                match [any-string! binary!] series
                not block? value
                not issue? value  ; want e.g. # adds as #{00} to BINARY!
                not integer? value
                (type of series) != (type of :value)  ; breaks /PART
            ] then [
                value: redbol.form/unspaced :value
            ]
        ]
    ]
]

append: emulate [oldsplicer onlify :append]
insert: emulate [oldsplicer onlify :insert]
change: emulate [oldsplicer onlify :change]


quote: emulate [:the]



; Rebol2 was extended ASCII-based, typically expected to be Latin1.  This
; means some files depended on being able to LOAD characters that were
; arbitrary bytes, representing the first 255 characters of unicode.
;
; Red, R3-Alpha, and Ren-C are UTF-8-based by default.  However, this means
; that some Rebol2 scripts which depend on reading Latin1 files will fail.
; One example is %pdf-maker.r, which embeds a Latin1 font metrics file as
; compressed data in the script itself.
;
; It's relatively unlikely that a Latin1 file using high-bit characters would
; decode as valid UTF-8:
;
; "To appear as a valid UTF-8 multi-byte sequence, a series of 2 to 4 extended
;  ASCII 8-bit characters would have to be an unusual combination of symbols
;  and accented letters (such as an accented vowel followed immediately by
;  certain punctuation). In short, real-world extended ASCII character
;  sequences which look like valid UTF-8 multi-byte sequences are unlikely."
;
; So what we do as a heuristic is to try UTF-8 first and fall back on Latin1
; interpretation.  This means bad UTF-8 input that isn't Latin1 will be
; misinterpreted...but since Rebol2 would accept any bytes, it's no worse.
;
hijack :ren.transcode enclose copy :ren.transcode helper [
    lambda [f [frame!]] [
        do copy f except e -> [ ; COPY so we can DO it again if needed
            if e.id != 'bad-utf8 [
                fail e
            ]

            f.source: copy f.source
            assert [binary? f.source]  ; invalid UTF-8 can't be in a string
            pos: f.source
            iterate pos [
                if pos.1 < 128 [continue]  ; ASCII
                if pos.1 < 192 [
                    insert pos #{C2}
                    pos: next pos
                    continue
                ]
                change pos pos.1 - 64
                insert pos #{C3}
                pos: next pos
            ]

            do f  ; this time if it fails, we won't trap it
        ]
    ]
]


call: emulate [  ; brings back the /WAIT switch (Ren-C waits by default)
    if undefined? 'call* [
        does [fail "CALL not available in web build"]
    ] else [
        get 'call*
    ]
]


to-integer: emulate [
    adapt :to-integer [  ; TO-INTEGER of #1 is nominally 1 in Ren-C
        if char? value [value: codepoint of value]
    ]
]


=== I/O ===

import <io2.r>

print: emulate [:print2]

split-path: emulate [:split-path-2]

read: emulate [:read2]
write: emulate [:write2]
load: emulate [:load2]


=== FINISH UP ===

; We do this last, just in case any of the above would accidentally use
; a Redbol-style path.  (Future plan is that FUNC2 would use a hooked evaluator
; that would have specialized handling.)
;
system.options.redbol-paths: true

; When the caller asks for `lib/append` or similar, that will give them the
; Redbol version as it was when they started.  If you want the versions from
; Ren-C, use `ren.append` etc
;
lib: redbol
