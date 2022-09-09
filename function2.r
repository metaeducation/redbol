Rebol [
    Title: "Rebol2 Compatibility for Functions and Apply Engine"
    File: %function2.r
    License: {LGPL 3.0}

    Type: module
    Name: Redbol-Function

    Exports: [
        func2 function2 does2 has2
        apply2
    ]

    Description: {
        This breaks out some of the function-related emulation code into a
        separate module, to try and make the redbol module more manageable.
    }
]

ren: lib  ; make it clearer when we are using the Ren-C library
lib: ~use-ren-instead-of-lib~


; Refinement arguments in Ren-C are conveyed via the refinement value itself:
;
; https://trello.com/c/DaVz9GG3/
;
; The old behavior is simulated by creating locals for the refinement args
; and then having a bit of code at the beginning of the body that moves the
; refinement's value into it.
;
; Also adds a specialization of the definitional return to act as EXIT.
;
rewrite-spec-and-body: func [
    return: "New spec" [block!]
    @body-out "New body" [block!]
    spec [block!]
    body [block!]
][
    spec: copy spec

    ; R3-Alpha didn't implement the Rebol2 `func [[throw catch] x y][...]`
    ; but it didn't error on the block in the first position.  It just
    ; ignored it.  For now, do the same in the emulation.
    ;
    if block? first spec [take spec]  ; skip Rebol2's [throw]

    ; Rebol2 and R3-Alpha hid refinements that appeared after /LOCAL
    ; https://forum.rebol.info/t/analogue-to-rebol2s-hidden-parameters/1273
    ; Don't hide them, but make them work by pushing them before <local>.
    ;
    let local-tag-pos: null

    let swap-if-after-local: does [
        if local-tag-pos [
            assert [local-tag-pos.1 = <local>]
            local-tag-pos: insert local-tag-pos ^ take spec
            assert [local-tag-pos.1 = <local>]
        ]
    ]

    let spool-descriptions-and-locals: does [
        while [match [text! set-word!] first spec] [  ; end-tolerant (null)
            if not set-word? spec.1 [
                swap-if-after-local  ; description for hidden refinement..?
            ]
            spec: my next
        ]
    ]

    while [not tail? spec] [
        refinement: to word! maybe match path! spec.1

        ; Refinements with multiple arguments are no longer allowed, and
        ; there weren't many of those so it's not a big deal.  But there
        ; are *many* instances of the non-refinement usage of /LOCAL.
        ; These translate in Ren-C to the <local> tag.
        ;
        if refinement = 'local [
            change spec <local>
            local-tag-pos: spec  ; see note about hidden refinements above
            spec: my next
            continue
        ]

        if not refinement [
            spec: my next  ; ordinary args (or local WORD! after /LOCAL)
            continue
        ]

        ; if we get here it's a refinement that is *not* /LOCAL.  This
        ; means if local-tag-pos isn't null, we need to be moving
        ; everything we do back to before the <local> (spool does this too)
        ;
        swap-if-after-local
        spec: my next

        spool-descriptions-and-locals

        if not let argument: match [word! lit-word! get-word!] first spec [
            insert body compose/deep [
                (refinement): either (refinement) [true] [blank]
            ]
            continue
        ]

        if tail? spec [break]

        take spec  ; don't want argument between refinement + type block

        spool-descriptions-and-locals

        ; may be at tail, if so need the [any-value!] injection

        if types: match block! spec.1 [  ; explicit arg types
            swap-if-after-local
            spec: my next
        ]
        else [
            insert spec [any-value!]  ; old refinement-arg default
            swap-if-after-local
            spec: my next
        ]

        append spec to tuple! argument  ; .VAR in specs are locals

        ; Take the value of the refinement and assign it to the argument
        ; name that was in the spec.  Then set refinement to true/blank.
        ;
        ; (Rebol2 missing refinements are #[none], or #[true] if present
        ; Red missing refinements are #[false], or #[true] if present
        ; Rebol2 and Red arguments to unused refinements are #[none]
        ; Since there's no agreement, Redbol goes with the Rebol2 way,
        ; since NONE! is closer to Ren-C's NULL for unused refinements.)

        insert body compose/deep [
            (argument): :(refinement)
            (refinement): either value? :(refinement) [true] [blank]
        ]

        if tail? spec [break]
        spool-descriptions-and-locals
        if tail? spec [break]

        if let extra: match any-word! first spec [
            fail [
                {Refinement} refinement {can't take more than one}
                {argument in the Redbol emulation, so} extra {must be}
                {done some other way.  (We should be *able* to do}
                {it via variadics, but would be much more involved.)}
            ]
        ]
    ]

    spec: head spec  ; At tail, so seek head for any debugging!

    body-out: compose [
        ;
        ; We don't go to an effort to provide a non-definitional return.
        ; But support for an EXIT that's a synonym for returning void.
        ;
        exit: specialize :return [value: '~]

        ; Historical Rebol supports an implicit RETURN, which was vetoed
        ; in the design of Ren-C (but easy to customize, just like this)
        ;
        ; https://forum.rebol.info/t/1656
        ;
        return (as group! body)
    ]
    append spec spread [<local> exit]  ; FUNC needs it (function doesn't...)
    return spec
]


func-nonconst: (
;    reskinned [body [block!]] adapt :func []

    :func
)

function-nonconst: (
;     reskinned [body [block!]] adapt :function []

    :function
)

func2: func [
    return: [action!]
    spec [block!]
    body [block!]
][
    if find spec <local> [  ; Red uses `return:` else it would be a good hint
        return lib.func spec body  ; assume "new style" function
    ]

    [spec body]: rewrite-spec-and-body spec body

    return func-nonconst spec body
]

function2: func [
    return: [action!]
    spec [block!]
    body [block!]
    /with [object! block! map!]  ; from R3-Alpha, not adopted by Red
    /extern [block!]  ; from R3-Alpha, adopted by Red
][
    if find spec <local> [  ; Red uses `return:` else it would be a good hint
        return lib.function spec body  ; assume "new style" function
    ]

    if block? with [with: make object! with]

    [spec body]: rewrite-spec-and-body (copy spec) (copy body)

    ; The shift in Ren-C is to remove the refinements from FUNCTION, and
    ; put everything into the spec dialect...marked with <tags>
    ;
    if with [
        append spec compose [<in> (with)]  ; <in> replaces /WITH
    ]
    if extern [
        append spec compose [<with> (spread extern)]  ; <with> replaces /EXTERN
    ]

    return function-nonconst spec body
]


does2: specialize :func2 [spec: []]

has2: lambda [
    vars [block!]
    body [block!]
][
    func2 (head of (insert copy vars '/local)) body
]


=== APPLY EMULATION ===

; Historical Rebol had an APPLY which would take refinements themselves as
; arguments in the block.
;
; `APPEND/ONLY/DUP A B 2` => `apply :append [a b none none true true 2]`
;
; This made the apply call aware of the ordering of refinements in the spec,
; which is not supposed to be a thing.  So Ren-C's APPLY requires you to
; account for any refinements in your call by naming them in the path that you
; are applying, then the array should have exactly that number of arguments:
;
; https://trello.com/c/P2HCcu0V
;
; This emulation is a good example of how FRAME! can be used to build
; customized apply-like functions.

apply2: func [
    return: [<opt> any-value!]
    action [action!]
    block [block!]
    /only
    <local> arg frame params using-args
][
    frame: make frame! :action
    params: parameters of :action
    using-args: true

    while [not tail? block] [
        block: if only [
            arg: block.1
            next block
        ] else [
            [arg @block]: evaluate block
        ]

        if refinement? params.1 [
            ;
            ; Ren-C allows LOGIC! to control parameterless refinements and
            ; canonizes to either null or #.  Rebol2 allowed any truthy
            ; thing, it does not allow BLANK!.  This makes a BLOCK!-style
            ; apply using positions non-viable.  We OPT all "nones" here.
            ;
            using-args: to-logic set (in frame second params.1) all [get 'arg, #]
        ] else [
            if using-args [  ; v-- should IN allow QUOTED?
                set (in frame noquote params.1) if meta-word? params.1 [
                    ^ get/any 'arg
                ] else [
                    get 'arg
                ]
            ]
        ]

        params: next maybe params
    ]

    comment [
        ; Too many arguments was not a problem for R3-alpha's APPLY, it
        ; would evaluate them all even if not used by the function.  It
        ; may or may not be better to have it be an error.

        if not tail? block [
            fail "Too many arguments passed in Redbol APPLY block."
        ]
    ]

    return do frame  ; nulls are optionals
]
