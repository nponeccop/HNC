{-# LANGUAGE FlexibleContexts #-}
module Utils.Options (err, O(..), Options(..), runOptions) where
import Control.Monad
import Control.Monad.Except
import System.Console.GetOpt
import System.Environment
import qualified Data.Map as M
import qualified Data.Set as S

data Options = OptBool String | OptString String String deriving Show

data O = O 
	{ oBool :: S.Set String
	, oString :: M.Map String String
  	, oNonOptions :: [String]
	} deriving Show

defaultO = O S.empty M.empty []

xxx = foldM f defaultO where
	f o (OptBool opt) = if S.member opt $ oBool o 
		then throwError $ "Duplicate " ++ opt ++ " option"
		else return o { oBool = S.insert opt (oBool o) }
	f o (OptString opt v) = if M.member opt $ oString o 
		then throwError $ "Duplicate " ++ opt ++ " option"
		else return  o { oString = M.insert opt v (oString o) }

getO (o, no) = do
	oo <- xxx o 
	return $ oo { oNonOptions = no }
    
compilerOpts options argv =
	case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs))

runOptions options bbb = getArgs >>= compilerOpts options >>= aaa >>= bbb

aaa x = case getO x of
	Left err -> ioError $ userError err
	Right o -> return o
	
err e = ioError $ userError e

