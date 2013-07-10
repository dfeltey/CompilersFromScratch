module Main where

import Tokens
import Parse
import Parser
import SExpr
import CEK
import System.Environment


main :: IO ()
main = do
	args <- getArgs
	case args of
		(progPath:_) -> do
				prog <- readFile progPath
				let tokens = lexS prog
				case tokens of
					Left err -> putStrLn err
					Right toks -> do
								let toks' = filter notSpace toks
								let parseTree = (unP parseExpr) toks'
								case parseTree of
									[] -> putStrLn "Program failed to parse"
									((sexpr,_):_) -> do
											let expr = desugar sexpr
											let cekCode = compile expr
											let val = run cekCode
											putStr $ (unlines . snd) val
		otherwise -> do
			prog <- getLine
			let tokens = lexS prog
			case tokens of
					Left err -> putStrLn err
					Right toks -> do
								let toks' = filter notSpace toks
								let parseTree = (unP parseExpr) toks'
								case parseTree of
									[] -> putStrLn "Program failed to parse"
									((sexpr,_):_) -> do
											let expr = desugar sexpr
											let cekCode = compile expr
											let val = run cekCode
											putStr $ (unlines . snd) val
											main
		
