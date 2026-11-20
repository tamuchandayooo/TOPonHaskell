{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.TestAnalyzer where

import Data.Text (Text,pack)
import Data.Maybe (mapMaybe)
import Text.Show.Pretty (ppShow)
import Language.Haskell.Exts
import Domain.TestInfo
import Infrastructure.Logger (putErrorLog)
import Infrastructure.FileManager (insertLine, insertLineInFile)


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

extractRhs :: [Text] -> [Decl l] -> [Rhs l]
extractRhs ids decls = 
    concat $ map (extractOneRhs ids) decls

extractRhsFromPath path = do
    res <- extractDecl path
    case res of
        Right decls -> writeFile "test1.txt" $ ppShow $ extractRhs (extractSpecTypeFromDecl decls) decls
        Left  err   -> putErrorLog (show err)

------------------------------------------------------------------------------------------------------

getExpFromRhs :: Rhs l -> Either String (Exp l)
getExpFromRhs (UnGuardedRhs _ ex) = Right ex
getExpFromRhs (GuardedRhss  _ _ ) = Left  "failed extracting exp from rhs"

getAppInfo :: Exp l -> (Exp l, [Exp l])
getAppInfo = go []
    where
        go args (App _ f x) = go (x:args) f
        go args ex          = (ex,args)

getDoInfo :: Exp l -> Either String [Stmt l]
getDoInfo (MDo _ stmts) = Right stmts
getDoInfo (Do  _ stmts) = Right stmts
getDoInfo _             = Left  "failed extracting do block stmt from exp"

getStringLiteral :: Exp l -> String
getStringLiteral (Lit _ (String _ str _)) = str
getStringLiteral _                        = ""
        
getFunctionName :: Exp l -> String
getFunctionName (Var _ (UnQual _ (Ident _ name))) = name
getFunctionName _                                 = ""

getLineRange :: SrcSpanInfo -> (Int, Int)
getLineRange info =
  let spanInfo = srcInfoSpan info
  in  (srcSpanStartLine spanInfo, srcSpanEndLine spanInfo)

-- non-exhaustive
getAssertionType :: Exp l -> (String, (Exp l, Exp l))
getAssertionType (            InfixApp _ lhs (QVarOp _ (UnQual _ (Ident _ str))) rhs)  = (str, (lhs, rhs))
getAssertionType (Paren    _ (InfixApp _ lhs (QVarOp _ (UnQual _ (Ident _ str))) rhs)) = (str, (lhs, rhs))

parseHspecStatement :: Exp l -> Either String (HspecStatement l)
parseHspecStatement ex = do
    (app, bdy) <- case ex of
        InfixApp _ lhs (QVarOp _ (UnQual _ (Symbol _ "$"))) rhs ->
            Right (getAppInfo lhs, rhs)

        App _ _ _ ->
            let app = getAppInfo ex
            in case snd app of
                (_ : bdy : _) -> Right (app, bdy)
                _              -> Left "failed parsing hspecStatement from exp"

        _ ->
            Left "failed parsing hspecStatement from exp"

    case (getFunctionName (fst app), snd app) of
        ("describe", str : _) ->
            Right $ Describe (getStringLiteral str) bdy
        ("it", str : _) ->
            Right $ It (getStringLiteral str) bdy
        ("context", str : _) ->
            Right $ Context (getStringLiteral str) bdy
        ("specify", str : _) ->
            Right $ Specify (getStringLiteral str) bdy

        

        _ ->
            Left "failed parsing hspecStatement from exp"    

-- ex     : Exp from Rhs
-- groups : list for groups
extractTests :: Exp SrcSpanInfo -> [String] -> [TestInfo]
extractTests ex groups =
  case parseHspecStatement ex of
    Right (Describe name body) -> extractTests body (groups ++ [name])
    Right (Context  name body) -> extractTests body (groups ++ [name])
    Right (It       name body) ->
        let (startLine, endLine) = getLineRange (ann ex)
            (assertion, (input,expected)) = getAssertionType body
        in  [TestInfo
            { testGroups    = map pack groups
            , testName      = pack $ name
            , input         = pack $ prettyPrint $ input
            , expected      = pack $ prettyPrint $ expected
            , assertion     = pack $ assertion
            , testStartLine = startLine
            , testEndLine   = endLine
            }
        ]
    Right (Specify  name body) ->
        let (startLine, endLine) = getLineRange (ann ex)
            (assertion, (input,expected)) = getAssertionType body
        in  [TestInfo
            { testGroups    = map pack groups
            , testName      = pack $ name
            , input         = pack $ prettyPrint $ input
            , expected      = pack $ prettyPrint $ expected
            , assertion     = pack $ assertion
            , testStartLine = startLine
            , testEndLine   = endLine
            }
        ]

    Left _ ->
      case getDoInfo ex of
        Right stmts -> concatMap extractFromStmt stmts
        Left  _     -> []
  where
    extractFromStmt stmt =
      case stmt of
        Qualifier _ e -> extractTests e groups
        _             -> []

extractTestsFromPath path = do
    res <- extractDecl path
    case res of
        Right decls -> do
            let rhs = extractRhs (extractSpecTypeFromDecl decls) decls
            case rhs of
                [] -> putErrorLog "No RHS found"
                rs -> mapM_ oneTest rs

        Left err -> putErrorLog (show err)
    where
        oneTest r =
            case getExpFromRhs r of
                Right ex -> putStrLn $ show $ (extractTests ex [])
                Left  er -> putErrorLog (show er)

insertInfo :: FilePath -> TestInfo -> IO ()
insertInfo path info = do
    insertLineInFile (testStartLine info) (infoTxt) path
    where
        infoTxt = "{-"
               <> "input     : " <> (input info) <> "\n"
               <> "expected  : " <> (expected info) <> "\n"
               <> "assertion : " <> (assertion info) <> "\n"
               <> "-}"

------------------------------------------------------------------------------------------------------