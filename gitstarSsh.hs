{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude hiding (lookup)
import Control.Monad.State

import SSH.Channel
import SSH.Crypto
import SSH.Session
import qualified SSH

import System.Environment
import System.FilePath
import System.Process
import System.IO

import Data.IORef
import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpClient

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Maybe

import Data.Binary.Get (runGet)
import Data.Bson.Binary (getDocument)

import Database.MongoDB  hiding (auth)

import Text.Regex.Posix


type L = L.ByteString
type S = S.ByteString

main :: IO ()
main = do
    port    <- read `liftM` getEnv "SSH_PORT" :: IO Int
    keyPath <- getEnv "SSH_KEY"
    hPutStrLn stderr $ "Starting ssh server on port "++ show port  ++
                       " with key " ++ keyPath ++ " ..."
    kp <- rsaKeyPairFromFile keyPath
    startSSH kp (fromIntegral port)
  where
    startSSH kp = SSH.start (sessionConfig kp) channelConfig
    sessionConfig kp = SessionConfig { scAuthMethods = ["publickey"]
                                     , scAuthorize = sshAuthorize
                                     , scKeyPair = kp }
    channelConfig = ChannelConfig { ccRequestHandler = channelRequest }

-- | Handle a request. Based on the example it seems like the @wr@
-- value is used a write-back flag.
channelRequest :: Bool -> ChannelRequest -> Channel ()
channelRequest wr (Execute cmd) = handleExecRequest wr cmd
channelRequest wr (Environment "LANG" _) = when wr channelSuccess
channelRequest wr r = do
  channelError $ "Unknown request: "++ show r ++ "\r\n" ++
                 "This server only accepts EXEC requests."
  when wr channelFail

--
-- Handle git-(upload|receive)-pack
--

-- | Top-level exec handler
handleExecRequest :: Bool -> String -> Channel ()
handleExecRequest wr cmd = do
  liftIO $ hPutStrLn stderr $ "Handling: " ++ cmd
  case words cmd of
    ["git-receive-pack", path] -> handleReceivePack wr path
    ("git-upload-pack":opts)   -> handleUploadPack wr opts
    _ -> errorWith wr $ "Invalid exec request: " ++ cmd

-- | Handle git-receive-pack
handleReceivePack :: Bool -> FilePath -> Channel ()
handleReceivePack wr path = do
  mRepoInfo <- pathToRepoInfo wr path
  case mRepoInfo of
    Nothing -> return ()
    Just (owner, repo) -> do
      rootPath <- liftIO $ getEnv "BASE_DIR"
      let fullPath = rootPath </>  owner </> repo
      ok <- checkWriteAccess owner repo
      if ok
        then execute . unwords $ ["git-receive-pack", fullPath]
        else errorWith wr $ "Insufficient access."


-- | Handle git-upload-pack
handleUploadPack :: Bool -> [String] -> Channel ()
handleUploadPack wr opts = do
  unless validOptions failBadOpts
  mRepoInfo <- pathToRepoInfo wr path
  when (validOptions && isJust mRepoInfo) $ do
    let (owner, repo) = fromJust mRepoInfo
    rootPath <- liftIO $ getEnv "BASE_DIR"
    let fullPath = rootPath </>  owner </> repo
    ok <- checkReadAccess owner repo
    if ok
      then execute . unwords $ ["git-upload-pack", newOpts, fullPath]
      else errorWith wr $ "Insufficient access."
  where path = safeLast opts
        -- Extract opts:
        newOpts = unwords $
          let haveStrict =  strict `elem` opts
              opts' = splitTout . filter (/= strict) $ safeInit opts
          in concat $
               [ if haveStrict then [strict] else []
               , case opts' of
                   ["--timeout=",tval] | tval =~ ("[0-9]+" :: String) ->
                        [tout ++ tval]
                   _ -> []
               ]
        --
        splitTout []     = []
        splitTout (o:os) = if o =~ (tout++"[0-9]+" :: String)
                             then tout : ( drop (length tout) o : os)
                             else o : os
        -- Make sure opts are of the form: [--strict] [--timeout=<n>]
        validOptions =
          let o = unwords $ safeInit opts
              re0 = "--strict(\\s+--timeout=\\s*[0-9]+)?"
              re1 = "--timeout=\\s*[0-9]+(\\s+--strict)?"
              re  = "^(|" ++ re0 ++ "|" ++ re1 ++")$" 
          in (o =~ (re :: String)) :: Bool
        --
        tout = "--timeout="
        --
        strict = "--strict"
        --
        safeLast xs | xs == []  = []
                    | otherwise = last xs
        --
        safeInit xs | length xs <= 1 = []
                    | otherwise      = init xs
        --
        failBadOpts = errorWith wr $ "Invalid options. Expected: " 
                  ++  "git-upload-pack [--strict] [--timeout=<n>] /user/repo"

--
-- Handle authentication and access control
--

-- | Very current user has read access to @/owner/pName@.
checkReadAccess :: String -> String -> Channel Bool
checkReadAccess owner pName = do
  usr <- gets csUser
  mp <- liftIO $ getProject owner pName
  return $ fromMaybe False $ isReader usr mp
    where isReader usr mp = do
            p   <- mp
            o   <- lookup "owner" p
            rs  <- lookup "readers" p
            isPub <- return $ case lookup "public" p of
                       Just v | v == False -> False
                              | otherwise  -> True
                       Nothing -> False
            cs <- lookup "collaborators" p
            let readerOrCollab = usr `elem` o:(rs ++ cs)
            return $ isPub || if usr == anonymous
                                then False
                                else readerOrCollab

-- | Very current user has write access to @/owner/pName@.
checkWriteAccess :: String -> String -> Channel Bool
checkWriteAccess owner pName = do
  usr <- gets csUser
  if usr == anonymous
    then return False -- Anonymous cannot write
    else do mp <- liftIO $ getProject owner pName
            return $ fromMaybe False $ isWriter usr mp
    where isWriter usr mp = do
            p  <- mp
            o  <- lookup "owner" p
            cs <- lookup "collaborators" p
            return $ usr `elem` o:cs

-- | Find a project given the owner and repo (project name).
getProject :: String -> String -> IO (Maybe Document)
getProject owner proj = do
  baseUrl <- getEnv "GITSTAR_URL"
  authUser <- getEnv "GITSTAR_USER"
  authHmac <- getEnv "GITSTAR_USER_HMAC"
  let req0 = (getRequest $ baseUrl ++ "/" ++ owner ++ "/" ++ noGitExt proj)
      hailsUserCookie = ("_hails_user", S8.pack authUser)
      hailsUserHmacCookie = ("_hails_user_hmac", S8.pack authHmac)
      accHdr  = ("Accept", "application/bson")
      req = req0 { reqHeaders = accHdr : reqHeaders req0
                 , reqCookies = [hailsUserCookie, hailsUserHmacCookie]}
  resp <- genSimpleHttp req L.empty Nothing 0 False
  if respStatus resp /= stat200
    then return Nothing
    else Just `liftM` bsonDocFromBody resp

-- | Construct Bson document from response body.
bsonDocFromBody :: HttpResp IO -> IO Document
bsonDocFromBody resp = do
  ref <- newIORef []
  (respBody resp |. maybeChunk) |$ (docI ref)
  readIORef ref
    where maybeChunk = if respChunk resp then inumToChunks else inumNop
          docI ref = do x <- pureI
                        liftIO $ writeIORef ref (decodeDoc x)
          decodeDoc = runGet getDocument

-- | Authentitcate a user.
sshAuthorize :: Authorize -> Session Bool
sshAuthorize (PublicKey uName key) = liftIO $ do
  hPutStr stderr $ "Authenticating " ++ uName ++ "..."
  if uName == anonymous
    then hPutStrLn stderr "" >> return True
    else do verifyOk <- verifyUserKey uName key
            if verifyOk 
              then hPutStrLn stderr "OK!"
              else hPutStrLn stderr "FAILED!"
            return verifyOk
sshAuthorize _ = do
  liftIO $ hPutStrLn stderr "Expected public-key authentication."
  return False

-- | Anonymous user can read public repos
anonymous :: String
anonymous = "anonymous"

-- | Find a user based and verify their public key.
verifyUserKey :: String -> PublicKey -> IO Bool
verifyUserKey uName key = do
  mkeys <- getUserKeys uName
  return $ case mkeys of 
    Nothing -> False
    Just keys -> let kVal = encodeKey key
                 in kVal `elem` keys

-- | Get all the user keys.
getUserKeys :: String -> IO (Maybe [S])
getUserKeys uName = do
  baseUrl <- getEnv "GITSTAR_URL"
  authUser <- getEnv "GITSTAR_USER"
  authHmac <- getEnv "GITSTAR_USER_HMAC"
  let req0 = (getRequest $ baseUrl ++ "/" ++ uName ++ "/keys")
      hailsUserCookie = ("_hails_user", S8.pack authUser)
      hailsUserHmacCookie = ("_hails_user_hmac", S8.pack authHmac)
      accHdr  = ("Accept", "application/bson")
      req = req0 { reqHeaders = accHdr : reqHeaders req0
                 , reqCookies = [hailsUserCookie, hailsUserHmacCookie]}
  resp <- genSimpleHttp req L.empty Nothing 0 False
  if respStatus resp /= stat200
    then return Nothing
    else do doc <- bsonDocFromBody resp
            return $ do (Array keyDocs) <- look "keys" doc
                        mapM extractKeyValue keyDocs
      where extractKeyValue (Doc d) = do
              (Bin (Binary kb)) <- look "value" d
              case S8.words kb of
                (_:k:_) -> return k
                [k]     -> return k
                _       -> Nothing
            extractKeyValue _       = Nothing
            

--
-- Misc helper
--

execute :: String -> Channel ()
execute = spawnProcess . runInteractiveCommand

strictify :: L -> S
strictify = S.concat . L.toChunks

-- | Encode a public key into a 'Binary' blob
encodeKey :: PublicKey -> S
encodeKey key = B64.encode . strictify . blob $ key

-- | Strip enclosing quotes.
stripQuotes :: String -> String
stripQuotes = lstrip . rstrip
  where lstrip s = case s of
          [] -> []
          (x:xs) -> if x == '\'' then xs else s
        rstrip = reverse . lstrip . reverse

-- | Print error to channel and finish.
errorWith :: Bool -> String -> Channel ()
errorWith wr msg = do
    liftIO $ hPutStrLn stderr msg
    channelError msg
    when wr channelSuccess
    channelDone

-- | Convert a path (e.g., @/user/repo.git@) to the corresponding
-- owner (@user@) and repository name (@repo.git@). Note @.git@ is
-- always added
pathToRepoInfo :: Bool -> FilePath -> Channel (Maybe (String, FilePath))
pathToRepoInfo wr path = 
  case splitDirectories (stripQuotes path) of
    ["/",user,repo] -> if (not $ wellformed user) && (not $ wellformed repo)
                        then failBadPath else return $ Just (user, wGitExt repo)
    [user,repo] -> if (not $ wellformed user) && (not $ wellformed repo)
                      then failBadPath else return $ Just (user, wGitExt repo)
    _ -> failBadPath
  where failBadPath = do errorWith wr "Invalid path. Expected form: user/repo"
                         return Nothing
        wellformed s = s =~ ("^[^\\.]+" :: String)

-- | Remove @.git@ from name, if any
noGitExt :: String -> String
noGitExt n = if takeExtension n == ".git" then dropExtension n else n

-- | Add @.git@ to name, if none
wGitExt :: String -> String
wGitExt n = addExtension (noGitExt n) ".git"
