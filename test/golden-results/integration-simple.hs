[ ( "test/assets/Simple.hs" , [ Ok [] , Ok [] ] )
, ( "test/assets/UnverifiedExamples.hs"
  , [ Ok []
    , Ok
        [ "No example result was provided. For example:" , "" , "test" ]
    , Ok
        [ "The example was incorrect and couldn't be verified."
        , ""
        , "We expected:"
        , "2"
        , ""
        , "but received"
        , "3"
        ]
    , Ok
        [ "The example was incorrect and couldn't be verified."
        , ""
        , "We expected:"
        , "[ 2 , 3 , 4 , 5 ]"
        , ""
        , "but received"
        , "[ 2 , 4 , 5 ]"
        ]
    ]
  )
, ( "test/assets/AllVariations.hs"
  , [ Ok [] , Ok [] , Ok [] , Ok [] , Ok [] ]
  )
]