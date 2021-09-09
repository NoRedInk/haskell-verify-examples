[ ( "test/assets/LanguageExtensions.hs" , [ Ok [] ] )
, ( "test/assets/InternalFunction.hs" , [ Ok [] ] )
, ( "test/assets/Simple.hs"
  , [ Ok []
    , Ok []
    , Ok
        [ "No example result was provided. For example:"
        , ""
        , "test + test"
        ]
    ]
  )
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
        , "[2,3,4,5]"
        , ""
        , "but received"
        , "[2,4,5]"
        ]
    ]
  )
, ( "test/assets/AllVariations.hs"
  , [ Ok [] , Ok [] , Ok [] , Ok [] , Ok [] ]
  )
, ( "test/assets/Headless.hs" , [ Ok [] ] )
, ( "test/assets/Imports.hs" , [ Ok [] , Ok [] , Ok [] ] )
]