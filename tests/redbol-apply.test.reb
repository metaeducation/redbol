; These are tests that came from the R3-Alpha suite for APPLY, that were then
; used on an emulated REDBOL-APPLY with Ren-C...and then translated back to
; Redbol conventions.

[#44 (
    error? try [apply 'append/only [copy [a b] 'c]]
)]
(1 == apply :subtract [2 1])
(1 = (apply :- [2 1]))

; !!! These were permitted by Rebol2 APPLY, as missing arguments were
; treated as #[none].  Ren-C's ~none~ concept is an isotope and cannot be
; taken by ordinary function arguments, so this would have to be passing
; BLANK! or NULL instead.  It's not clear why support for too few args
; would be desirable.
;
;    (null = apply func [a] [a] [])
;    (null = apply/only func [a] [a] [])

[#2237
    (error? try [apply func [a] [a] [1 2]])
    (error? try [apply/only func [a] [a] [1 2]])
]

(error? apply :make [error! ""])

(# = apply func [/a] [a] [#[true]])
(null = apply func [/a] [a] [#[false]])
(null = apply func [/a] [a] [])
(# = apply/only func [/a] [a] [#[true]])

(
    comment {The WORD! false, not #[false], but allowed in Rebol2}

    # = apply/only func [/a] [a] [false]
)
(null == apply/only func [/a] [a] [])
(use [a] [a: true # = apply func [/a] [a] [a]])
(use [a] [a: false null == apply func [/a] [a] [a]])
(use [a] [a: false # = apply func [/a] [a] [/a]])
(use [a] [a: false /a = apply/only func [/a] [/a] [/a]])
(paren! == apply/only :type? [()])
([1] == head apply :insert [copy [] [1] none none])
([1] == head apply :insert [copy [] [1] none none none false])
([[1]] == head apply :insert [copy [] [1] none none none true])
(activation? apply :type? [:print])
(get-word! == apply/only :type? [:print])

[
    #1760

    (1 == do func [] [apply does [] [return 1] 2])
    (1 == do func [] [apply func [a] [a] [return 1] 2])
    (1 == do func [] [apply does [] [return 1]])
    (1 == do func [] [apply func [a] [a] [return 1]])
    (1 == do func [] [apply func [a b] [a] [return 1 2]])
    (1 == do func [] [apply func [a b] [a] [2 return 1]])
]

(
    none? apply func [
        x [any-type!]
    ][
        get 'x
    ][
        none
    ]
)
(
    none? apply func [
        'x [any-type!]
    ][
        get 'x
    ][
        none
    ]
)
(
    none? apply func [
        return: [any-type!]
        x [any-type!]
    ][
        return get 'x
    ][
        none
    ]
)
(
    unset? apply func [
        return: [any-type!]
        'x [any-type!]
    ][
        return get 'x
    ][
        ~
    ]
)
(
    error? apply func ['x [any-type!]] [
        return get 'x
    ][
        make error! ""
    ]
)
(
    error? apply/only func [x [any-type!]] [
        return get 'x
    ] head insert copy [] make error! ""
)
(
    error? apply/only func ['x [any-type!]] [
        return get 'x
    ] head insert copy [] make error! ""
)
(use [x] [x: 1 strict-equal? 1 apply func ['x] [:x] [:x]])
(use [x] [x: 1 strict-equal? 1 apply func ['x] [:x] [:x]])
(
    use [x] [
        x: 1
        strict-equal? first [:x] apply/only func [:x] [:x] [:x]
    ]
)
(
    use [x] [
        x: ~
        strict-equal? ':x apply/only func ['x [any-type!]] [
            return get 'x
        ] [:x]
    ]
)
(use [x] [x: 1 strict-equal? 1 apply func [:x] [:x] [x]])
(use [x] [x: 1 strict-equal? 'x apply func [:x] [:x] ['x]])
(use [x] [x: 1 strict-equal? 'x apply/only func [:x] [:x] [x]])
(use [x] [x: 1 strict-equal? 'x apply/only func [:x] [return :x] [x]])
(
    use [x] [
        unset 'x
        strict-equal? 'x apply/only func ['x [any-type!]] [
            return get 'x
        ] [x]
    ]
)

[(
    comment {MAKE FRAME! :RETURN should preserve binding in the frame}
    1 == do func [] [apply :return [1] 2]
)]

(null == apply/only func [/a] [a] [#[false]])
(paren! == apply/only :type? [()])
`
