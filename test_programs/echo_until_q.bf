++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
++++++++++
+++                             #0: store 'q'
> +                             #1: main loop flag
                                #2: user input
                                #3: equals flag
                                #4: q copy 1 / absolute diff
                                #5: q copy 2
                                #6: input copy 1
                                #7: input copy 2
                                #8: equals copy 1
                                #9: equals copy 2

[                               loop on #1

    > ,                         read input to #2

                                copy q from #0 to #4 (leaving #5 at 0)
    << [>>>>+>+<<<<<-]          move #0 to #4 and #5
    >>>>> [<<<<<+>>>>>-]        move #5 to #0

                                copy input from #2 to #6 (leaving #7 at 0)
    <<< [>>>>+>+<<<<<-]         move #2 to #6 and #7
    >>>>> [<<<<<+>>>>>-]        move #7 to #2

    < [                         loop on #6

                                IF START

        <<< +                   #3: set equals flag to 1

                                copy #4 from #4 to #8 (leaving #9 at 0)
        > [>>>>+>+<<<<<-]       move #4 to #8 and #9
        >>>>> [<<<<<+>>>>>-]    move #9 to #4

        < [<<<<< - >>>>> [-]]   loop on #8: set equals flag (#3) to 0 if x != 0

        <<<<< [                 loop on #3

                                if #4 == 0
            >>> [ << + >> - ]       loop on #6: move rest of #6 to #4
            +                       increment both
            << +

            < -
        ]
                                IF END

        > -
        >> -                    decrement both
    ]

    <<<<< -
    >>> [                        loop on #4

        <<.>>

        <<< + >>>
        [-]
    ]

    <<<
]
