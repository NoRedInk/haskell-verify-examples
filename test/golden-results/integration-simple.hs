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
, ( "test/assets/Imports.hs"
  , [ Err
        "WontCompile\n  [ GhcError\n      { errMsg =\n          \"/tmp/hint-417e79abc3343d6c/M216882775353538971121907647059100092774320821907.hs:2:20: error:\\n    parse error on input \\8216>>\\8217\"\n      }\n  ]"
    ]
  )
]