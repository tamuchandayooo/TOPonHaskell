{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.TestAnalyzer where

import Data.Text (Text,pack)
import Data.Maybe (mapMaybe)
import Language.Haskell.Exts
import Infrastructure.Logger (putErrorLog)
import Text.Show.Pretty (ppShow)

import Domain.TestInfo
import Foreign (allocaArray)

------------------------------------------------------------------------------------------------------

extractDecl :: FilePath -> IO (Either String [Decl SrcSpanInfo])
extractDecl path = do
    result <- parseFile path
    case result of
        ParseOk (Module _ _ _ _ decls) -> pure $ Right decls
        ParseOk _                      -> pure $ Left  "not a Module"
        ParseFailed _ _                -> pure $ Left  "parse failed"

------------------------------------------------------------------------------------------------------

extractSpecType :: FilePath -> IO (Either String [Text])
extractSpecType path = do
    result <- extractDecl path
    case result of
        Right decls -> pure $ Right $ extractSpecTypeFromDecl decls
        Left  err   -> pure $ Left  err

extractSpecTypeFromDecl :: [Decl SrcSpanInfo] -> [Text]
extractSpecTypeFromDecl decls = mapMaybe specType decls
    where
        specType decl = case decl of
            TypeSig _ ([Ident _ id]) (TyCon _ (UnQual _ (Ident _ "Spec"))) -> Just $ pack id
            _                                                              -> Nothing

------------------------------------------------------------------------------------------------------

extractOneRhs :: [Text] -> Decl l -> [Rhs l]
extractOneRhs ids decl = 
    mapMaybe (rhs decl) ids
    where
        rhs decl id = 
            case decl of
                (PatBind _ (PVar _ (Ident  _ name)) rhs _)
                    | pack name == id -> Just rhs
                _                     -> Nothing

extractRhs ids decls = 
    concat $ map (extractOneRhs ids) decls

extractRhsFromPath path = do
    res <- extractDecl path
    case res of
        Right decls -> writeFile "test1.txt" $ ppShow $ extractRhs (extractSpecTypeFromDecl decls) decls
        Left  err   -> putErrorLog "error"

------------------------------------------------------------------------------------------------------







------------------------------------------------------------------------------------------------------


aaa = 
    [ UnGuardedRhs
        SrcSpanInfo
        { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 7 14 27
        , srcInfoPoints = [ SrcSpan "test\\Spec.hs" 12 7 12 8 ]
        }
        (InfixApp
        SrcSpanInfo
            { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 9 14 27
            , srcInfoPoints = []
            }
        (App
            SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 9 12 31
                , srcInfoPoints = []
                }
            (Var
                SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 9 12 17
                , srcInfoPoints = []
                }
                (UnQual
                    SrcSpanInfo
                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 9 12 17
                    , srcInfoPoints = []
                    }
                    (Ident
                    SrcSpanInfo
                        { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 9 12 17
                        , srcInfoPoints = []
                        }
                    "describe")))
            (Lit
                SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 18 12 31
                , srcInfoPoints = []
                }
                (String
                    SrcSpanInfo
                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 18 12 31
                    , srcInfoPoints = []
                    }
                    "description"
                    "description")))
        (QVarOp
            SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 32 12 33
                , srcInfoPoints = []
                }
            (UnQual
                SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 32 12 33
                , srcInfoPoints = []
                }
                (Symbol
                    SrcSpanInfo
                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 32 12 33
                    , srcInfoPoints = []
                    }
                    "$")))
        (Do
            SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 12 34 14 27
                , srcInfoPoints =
                    [ SrcSpan "test\\Spec.hs" 12 34 12 36
                    , SrcSpan "test\\Spec.hs" 13 5 13 5
                    , SrcSpan "test\\Spec.hs" 16 1 16 0
                    ]
                }
            [ Qualifier
                SrcSpanInfo
                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 13 5 14 27
                    , srcInfoPoints = []
                    }
                (InfixApp
                    SrcSpanInfo
                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 13 5 14 27
                    , srcInfoPoints = []
                    }
                    (App
                        SrcSpanInfo
                        { srcInfoSpan = SrcSpan "test\\Spec.hs" 13 5 13 19
                        , srcInfoPoints = []
                        }
                        (Var
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 13 5 13 7
                            , srcInfoPoints = []
                            }
                        (UnQual
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 13 5 13 7
                                , srcInfoPoints = []
                                }
                            (Ident
                                SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 13 5 13 7
                                , srcInfoPoints = []
                                }
                                "it")))
                        (Lit
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 13 8 13 19
                            , srcInfoPoints = []
                            }
                        (String
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 13 8 13 19
                                , srcInfoPoints = []
                                }
                            "is test 1"
                            "is test 1")))
                    (QVarOp
                        SrcSpanInfo
                        { srcInfoSpan = SrcSpan "test\\Spec.hs" 13 20 13 21
                        , srcInfoPoints = []
                        }
                        (UnQual
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 13 20 13 21
                            , srcInfoPoints = []
                            }
                        (Symbol
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 13 20 13 21
                                , srcInfoPoints = []
                                }
                            "$")))
                    (InfixApp
                        SrcSpanInfo
                        { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 9 14 27
                        , srcInfoPoints = []
                        }
                        (Lit
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 9 14 10
                            , srcInfoPoints = []
                            }
                        (Int
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 9 14 10
                                , srcInfoPoints = []
                                }
                            1
                            "1"))
                        (QVarOp
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 11 14 12
                            , srcInfoPoints = []
                            }
                        (UnQual
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 11 14 12
                                , srcInfoPoints = []
                                }
                            (Symbol
                                SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 11 14 12
                                , srcInfoPoints = []
                                }
                                "+")))
                        (InfixApp
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 13 14 27
                            , srcInfoPoints = []
                            }
                        (Lit
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 13 14 14
                                , srcInfoPoints = []
                                }
                            (Int
                                SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 13 14 14
                                , srcInfoPoints = []
                                }
                                1
                                "1"))
                        (QVarOp
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 15 14 25
                                , srcInfoPoints =
                                    [ SrcSpan "test\\Spec.hs" 14 15 14 16
                                    , SrcSpan "test\\Spec.hs" 14 16 14 24
                                    , SrcSpan "test\\Spec.hs" 14 24 14 25
                                    ]
                                }
                            (UnQual
                                SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 15 14 25
                                , srcInfoPoints =
                                    [ SrcSpan "test\\Spec.hs" 14 15 14 16
                                    , SrcSpan "test\\Spec.hs" 14 16 14 24
                                    , SrcSpan "test\\Spec.hs" 14 24 14 25
                                    ]
                                }
                                (Ident
                                    SrcSpanInfo
                                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 16 14 24
                                    , srcInfoPoints = []
                                    }
                                    "shouldBe")))
                        (Lit
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 26 14 27
                                , srcInfoPoints = []
                                }
                            (Int
                                SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 14 26 14 27
                                , srcInfoPoints = []
                                }
                                2
                                "2")))))
            ]))
            
    , UnGuardedRhs
        SrcSpanInfo
        { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 7 19 37
        , srcInfoPoints = [ SrcSpan "test\\Spec.hs" 17 7 17 8 ]
        }
        (InfixApp
        SrcSpanInfo
            { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 9 19 37
            , srcInfoPoints = []
            }
        (App
            SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 9 17 26
                , srcInfoPoints = []
                }
            (Var
                SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 9 17 16
                , srcInfoPoints = []
                }
                (UnQual
                    SrcSpanInfo
                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 9 17 16
                    , srcInfoPoints = []
                    }
                    (Ident
                    SrcSpanInfo
                        { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 9 17 16
                        , srcInfoPoints = []
                        }
                    "context")))
            (Lit
                SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 17 17 26
                , srcInfoPoints = []
                }
                (String
                    SrcSpanInfo
                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 17 17 26
                    , srcInfoPoints = []
                    }
                    "context"
                    "context")))
        (QVarOp
            SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 27 17 28
                , srcInfoPoints = []
                }
            (UnQual
                SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 27 17 28
                , srcInfoPoints = []
                }
                (Symbol
                    SrcSpanInfo
                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 27 17 28
                    , srcInfoPoints = []
                    }
                    "$")))
        (Do
            SrcSpanInfo
                { srcInfoSpan = SrcSpan "test\\Spec.hs" 17 29 19 37
                , srcInfoPoints =
                    [ SrcSpan "test\\Spec.hs" 17 29 17 31
                    , SrcSpan "test\\Spec.hs" 18 5 18 5
                    , SrcSpan "test\\Spec.hs" 20 1 20 0
                    ]
                }
            [ Qualifier
                SrcSpanInfo
                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 18 5 19 37
                    , srcInfoPoints = []
                    }
                (InfixApp
                    SrcSpanInfo
                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 18 5 19 37
                    , srcInfoPoints = []
                    }
                    (App
                        SrcSpanInfo
                        { srcInfoSpan = SrcSpan "test\\Spec.hs" 18 5 18 21
                        , srcInfoPoints = []
                        }
                        (Var
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 18 5 18 12
                            , srcInfoPoints = []
                            }
                        (UnQual
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 18 5 18 12
                                , srcInfoPoints = []
                                }
                            (Ident
                                SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 18 5 18 12
                                , srcInfoPoints = []
                                }
                                "specify")))
                        (Lit
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 18 13 18 21
                            , srcInfoPoints = []
                            }
                        (String
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 18 13 18 21
                                , srcInfoPoints = []
                                }
                            "test 2"
                            "test 2")))
                    (QVarOp
                        SrcSpanInfo
                        { srcInfoSpan = SrcSpan "test\\Spec.hs" 18 22 18 23
                        , srcInfoPoints = []
                        }
                        (UnQual
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 18 22 18 23
                            , srcInfoPoints = []
                            }
                        (Symbol
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 18 22 18 23
                                , srcInfoPoints = []
                                }
                            "$")))
                    (InfixApp
                        SrcSpanInfo
                        { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 9 19 37
                        , srcInfoPoints = []
                        }
                        (Lit
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 9 19 10
                            , srcInfoPoints = []
                            }
                        (Int
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 9 19 10
                                , srcInfoPoints = []
                                }
                            1
                            "1"))
                        (QVarOp
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 11 19 12
                            , srcInfoPoints = []
                            }
                        (UnQual
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 11 19 12
                                , srcInfoPoints = []
                                }
                            (Symbol
                                SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 11 19 12
                                , srcInfoPoints = []
                                }
                                "+")))
                        (InfixApp
                        SrcSpanInfo
                            { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 13 19 37
                            , srcInfoPoints = []
                            }
                        (Lit
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 13 19 14
                                , srcInfoPoints = []
                                }
                            (Int
                                SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 13 19 14
                                , srcInfoPoints = []
                                }
                                1
                                "1"))
                        (QVarOp
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 15 19 30
                                , srcInfoPoints =
                                    [ SrcSpan "test\\Spec.hs" 19 15 19 16
                                    , SrcSpan "test\\Spec.hs" 19 16 19 29
                                    , SrcSpan "test\\Spec.hs" 19 29 19 30
                                    ]
                                }
                            (UnQual
                                SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 15 19 30
                                , srcInfoPoints =
                                    [ SrcSpan "test\\Spec.hs" 19 15 19 16
                                    , SrcSpan "test\\Spec.hs" 19 16 19 29
                                    , SrcSpan "test\\Spec.hs" 19 29 19 30
                                    ]
                                }
                                (Ident
                                    SrcSpanInfo
                                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 16 19 29
                                    , srcInfoPoints = []
                                    }
                                    "shouldSatisfy")))
                        (RightSection
                            SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 31 19 37
                                , srcInfoPoints =
                                    [ SrcSpan "test\\Spec.hs" 19 31 19 32
                                    , SrcSpan "test\\Spec.hs" 19 36 19 37
                                    ]
                                }
                            (QVarOp
                                SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 32 19 34
                                , srcInfoPoints = []
                                }
                                (UnQual
                                    SrcSpanInfo
                                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 32 19 34
                                    , srcInfoPoints = []
                                    }
                                    (Symbol
                                    SrcSpanInfo
                                        { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 32 19 34
                                        , srcInfoPoints = []
                                        }
                                    "==")))
                            (Lit
                                SrcSpanInfo
                                { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 35 19 36
                                , srcInfoPoints = []
                                }
                                (Int
                                    SrcSpanInfo
                                    { srcInfoSpan = SrcSpan "test\\Spec.hs" 19 35 19 36
                                    , srcInfoPoints = []
                                    }
                                    2
                                    "2"))))))
            ]))
    ]



{-
done : Spec型(TypeSig)を探して，その識別子を返す関数
done : 指定した識別子(PVar)の定義の右辺(Rhs)を取り出す関数
-}

