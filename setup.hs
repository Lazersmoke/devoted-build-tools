#!/usr/bin/env stack
-- stack --install-ghc runghc
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.List (isInfixOf)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment
import System.Process
import Control.Monad
import System.Directory
import System.FilePath
import Text.Parsec
import Prelude hiding (FilePath)

data Config = Config
  {command :: Config -> IO ()
  ,mcVer :: String
  ,rootDir :: FilePath
  }

type Parser = Parsec String ()

parseConfig :: Parser Config
parseConfig = Config <$> parseCommand <*> lookAhead parseVersion <*> lookAhead parseRoot

parseCommand :: Parser (Config -> IO ())
parseCommand = choice $ map (\(s,x) -> string s *> spaces *> pure x)
  [("clean",cleanup)
  ,("stop",stopServer)
  ,("attach",attachServer)
  ,("start",startServer)
  ,("install",installServer)
  ]

parseVersion :: Parser String
parseVersion = option "1.10.2" . try $ manyTill anyChar (try $ string "--version") *> spaces *> choice (map string supportedMCVersions)

parseRoot :: Parser String
parseRoot = option "" . try $ manyTill anyChar (try $ string "--install-dir") *> spaces *> many1 (noneOf [' ','\n'])

supportedMCVersions :: [String]
supportedMCVersions =
  ["1.11.2"
  ,"1.11"
  ,"1.10.2"
  ,"1.8.8"
  ,"1.8.7"
  ]

devotedDir :: Config -> FilePath
devotedDir (Config {rootDir}) = rootDir </> "devoted"

spigotDir :: Config -> FilePath
spigotDir (Config {rootDir}) = rootDir </> "devoted-spigot"

spigotBuild :: Config -> FilePath
spigotBuild (Config {rootDir}) = rootDir </> "spigot-build"

-- print is a horrible hack to prevent spigotBuild and spigotDir from being forced while current directory is devotedDir
main :: IO ()
main = getArgs >>= \a -> case parse parseConfig "command line" (unwords a) of
  Left e -> do
    print e
    putStrLn "Invalid arguments. Try `./setup.hs clean` or `./setup.hs install --version <Minecraft Version>`"
  -- Make the rootDir independent of cwd
  Right c@(Config {rootDir,command}) -> makeAbsolute rootDir >>= \rd -> command (c {rootDir = rd})

installServer :: Config -> IO ()
installServer c@(Config {rootDir}) = do
  -- If the are giving us a minecraft version, check it, warning them if it is unsupported
  --ensure (return $ mcVer `elem` supportedMCVersions) unsupportedVer $
  -- Make sure they didn't already run the script and create the files
  ensure (not <$> doesDirectoryExist (devotedDir c)) alreadyExists $ do
    -- If all is well, proceed with the setup
    -- Download Mojangcraft and set it up
    installVanilla c
    -- Replace Mojangcraft with Spigot
    buildSpigot c
    putStrLn "Done!"
  where
  -- If they gave us an unsupported (or non-sense) version, tell them so
    --unsupportedVer = "Unsupported Minecraft version: \"" ++ show mcVer ++ "\". Try one of: \n" ++ unlines supportedMCVersions
    -- If they already set it up, warn and exit
    alreadyExists = "Warning: `devoted` folder already exists. Please run `./setup.hs clean` or remove it manually before reinstalling"

cleanup :: Config -> IO ()
cleanup c = do
  -- Delete all directories, making sure they exist so that we don't get errors
  forM_ [devotedDir c,spigotDir c,spigotBuild c] $ \x -> doesDirectoryExist x >>= flip when (removeDirectoryRecursive x)
  putStrLn "All clean!"

installVanilla :: Config -> IO ()
installVanilla c@(Config {rootDir,mcVer}) = do
  -- Create the directory we will install into
  createDirectoryIfMissing True (devotedDir c)
  createDirectoryIfMissing True (devotedDir c </> "plugins")
  -- Grab Mojangcraft jar from the offical download link
  -- Injection here is impossible because `elem mcVer supportedMCVersions`
  callProcess "wget" ["https://s3.amazonaws.com/Minecraft.Download/versions/" ++ mcVer ++ "/minecraft_server." ++ mcVer ++ ".jar","-O",devotedDir c </> "minecraft_server.jar"]
  -- Premptively agree to the EULA. Please no murder me M$ :3
  writeFile (devotedDir c </> "eula.txt") "eula=true"
  -- Let the server initialize. The important thing is that it accepts our eula agreement
  startServer c
  stopServer c

buildSpigot :: Config -> IO ()
buildSpigot c@(Config {mcVer}) = do
  -- Create and enter the build directory
  createDirectoryIfMissing True (spigotBuild c)
  -- Get the spigot build tools ready
  callCommand $ "wget https://hub.spigotmc.org/jenkins/job/BuildTools/lastSuccessfulBuild/artifact/target/BuildTools.jar -O " ++ (spigotBuild c) </> "BuildTools.jar"
  -- Spigot build tools like to have this unset for whatever reason; discard the exit code cause it can be 5 for some reason /shrug
  _ <- waitForProcess =<< spawnCommand "git config --global --unset core.autocrlf"
  -- Go into the build directory so BuildTools.jar puts its files in the right place
  setCurrentDirectory (spigotBuild c)
  -- Actually build Spigot
  putStrLn "Building spigot, might take a while"
  callProcess "java" ["-Xmx1500M","-jar","BuildTools.jar","--rev",mcVer]
  -- Make sure that it compiled properly and actually generated an artifact
  let spigotJar = "spigot-" ++ mcVer ++ ".jar"
  let cbJar = "craftbukkit-" ++ mcVer ++ ".jar"
  -- Ensure the build worked
  ensure (doesFileExist (spigotBuild c </> spigotJar)) "Failed to compile Spigot" $ do
    -- Create the spigot artifact storage folder
    createDirectoryIfMissing True (spigotDir c)
    -- Install the Spigot jar into the minecraft server
    copyFile (spigotBuild c </> spigotJar) (devotedDir c </> "minecraft_server.jar")
    -- Archive the Spigot and Craftbukkit jars
    copyFile (spigotBuild c </> spigotJar) (spigotDir c </> spigotJar)
    copyFile (spigotBuild c </> cbJar) (spigotDir c </> cbJar)
    -- Install Spigot and CB pom files
    copyFile (spigotBuild c </> "Spigot/Spigot-Server/pom.xml") (spigotDir c </> "spigot-pom.xml")
    copyFile (spigotBuild c </> "CraftBukkit/pom.xml") (spigotDir c </> "craftbukkit-pom.xml")
    -- Install Spigot jars into Maven
    callProcess "mvn" ["install:install-file","-Dfile=" ++ spigotBuild c </> spigotJar,"-Dpackaging=jar","-DpomFile=" ++ spigotBuild c </> "Spigot/Spigot-Server/pom.xml"]
    callProcess "mvn" ["install:install-file","-Dfile=" ++ spigotBuild c </> cbJar,"-Dpackaging=jar","-DpomFile=" ++ spigotBuild c </> "CraftBukkit/pom.xml"]

-- Server directory must exist in order to start the server
startServer :: Config -> IO ()
startServer c@(Config {rootDir}) = ensureServerExists c $ do
  -- Go into server directory so screen can find `minecraft_server.jar`
  setCurrentDirectory (devotedDir c)
  putStrLn "Starting Server..."
  -- `-dmS` is dameon mode; screen will run in the background. To control it manually, see `attachServer`
  callCommand $ "screen -dmS mc_screen -p 0 bash -c 'java -Xmx1500M -Xms500M -XX:MaxPermSize=256M -jar minecraft_server.jar nogui'"

-- The server must be up in order to attach to it
attachServer :: Config -> IO ()
attachServer c = ensureServerUp c $ do
  putStrLn "Attaching to Server..."
  -- `-r` will attach the console to the server screen
  callCommand $ "screen -S mc_screen -p 0 -r"

-- Stop the server (the server must be up to do this)
stopServer :: Config -> IO ()
stopServer c = ensureServerUp c $ do
  putStrLn "Stopping Server..."
  -- `-X stuff 'stop\n'` will push the text "stop" and newline into screen, issuing the /stop command to the server
  callCommand $ "screen -S mc_screen -p 0 -X stuff 'stop\n'"

-- The server being up ~= screen lists "mc_screen" as one of the present screens
ensureServerUp :: Config -> IO () -> IO ()
ensureServerUp c = ensureServerExists c . ensure (isInfixOf "mc_screen" . (\(_,a,_) -> a) <$> readProcessWithExitCode "screen" ["-list"] "") "Server is not running!"

-- The server existing ~= the server directory exists
ensureServerExists :: Config -> IO () -> IO ()
ensureServerExists c = ensure (doesDirectoryExist $ devotedDir c) $ "\"" ++ devotedDir c ++ "\" does not exist! Try running `./setup.hs install <Minecraft Version>` to setup the server."

-- Ensure some invariant, on threat of printing an error message and not calling the continuation
ensure :: IO Bool -> String -> IO () -> IO ()
ensure p f x = p >>= \b -> if b then x else putStrLn f
