--to do:
--OPTIONAL:
--do forms + button using blaze
--pretty up pages 

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty as S
import System.Directory
import Control.Monad.IO.Class
import Data.Time
import Data.List


import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R

import Data.Text.Lazy 


data BlogPost = BlogPost { title :: Text  
                        , textBody :: Text  
                        , date :: Text       
			, url :: Text       	                
                        } deriving (Show, Read)

authenticate :: Text -> Bool
authenticate p 
	| p == "password" = True
	| otherwise = False

--writes a BlogPost to a file stored in ./blogs
newFile :: BlogPost -> IO ()
newFile n = do 
  writeFile urlOfFile $ show n
  where urlOfFile = "./blogs/" ++ (Data.Text.Lazy.unpack $ url n) 

--takes a list of file content (as strings) where each element of the list corresponds to the contents of a file in ./blogs -> "reads" each one to get back to BlogPost type and returns them all as one piece of concatenated text
showBPs :: [String] -> Text
showBPs list = (htmlText<>createButton)
  where b = Prelude.map read list :: [BlogPost] 
        c = Prelude.map showBlogPost b
        htmlText = Data.Text.Lazy.concat c
        createButton = Data.Text.Lazy.pack "<button type= \"button\" onclick=\"document.location.href='/authen'\">Create New Post!</button>"

--does the same as above func (showBPs) but only concats the titles (as href links)
showAllTitles :: [String] -> Text
showAllTitles list = (htmlText<>createButton)
  where b = Prelude.map read list :: [BlogPost] 
        c = Prelude.map showTitle b
        htmlText = Data.Text.Lazy.concat c
        createButton = Data.Text.Lazy.pack "<button type= \"button\" onclick=\"document.location.href='/authen'\">Create New Post!</button>"

--given a list of file paths (date stamps as strings), cleans them up a bit and sorts them -> gives us a list ordered according to creation date + time of post 
getFiles :: [FilePath] -> [Text]
getFiles fileNames = filtered
    where splitted = splitOn (Data.Text.Lazy.pack ",") (Data.Text.Lazy.pack $ show fileNames)
          filtered = Data.List.sort $ Prelude.map (Data.Text.Lazy.filter (not . (`elem` ['"','[','/',']']))) splitted

--shows latest five blog posts by reversing list from getFiles and taking 5
listLatestFive :: ActionM()
listLatestFive = do
  _ <- liftIO $ createDirectoryIfMissing False "./blogs" 
  fileNames <- liftIO $ listDirectory "./blogs"
  let createButton = Data.Text.Lazy.pack "<button type= \"button\" onclick=\"document.location.href='/authen'\">Create New Post!</button>"
  if fileNames == []
    then html $ createButton
  else
    do
      _ <- liftIO $ setCurrentDirectory "./blogs"
      let listToPrint = Prelude.take 5 $ Prelude.reverse (getFiles fileNames)
      a <- liftIO $ mapM readFile $ Prelude.map Data.Text.Lazy.unpack listToPrint
      _ <- liftIO $ setCurrentDirectory "./.." 
      html $ (showBPs a)    
    
--lists all blog posts by showing titles as links in a date sorted list 
listAll :: ActionM()
listAll = do
  _ <- liftIO $ createDirectoryIfMissing False "./blogs"
  fileNames <- liftIO $ listDirectory "./blogs"
  let createButton = Data.Text.Lazy.pack "<button type= \"button\" onclick=\"document.location.href='/authen'\">Create New Post!</button>"
  if fileNames == []     
    then html $ createButton
    else 
      do
        _ <- liftIO $ setCurrentDirectory "./blogs"
        a <- liftIO $ mapM readFile $ Prelude.map Data.Text.Lazy.unpack (getFiles fileNames)
        _ <- liftIO $ setCurrentDirectory "./.." 
        html $ (showAllTitles a) 

--given a url, give back blog post as string
retrieveBlogPost :: Text -> ActionM String
retrieveBlogPost url = do
  blog <- liftIO $ readFile ("./blogs/" ++ Data.Text.Lazy.unpack url)
  return blog 	

--html to show title with href
showTitle :: BlogPost -> Text
showTitle post = do
  R.renderHtml $ do
    H.body $ do
     let filteredUrl = Data.Text.Lazy.filter (not . (`elem` ['"','[','/',']','\\'])) $ Data.Text.Lazy.pack $ show (url post)
     let urlLink = H.toValue (Data.Text.Lazy.unpack filteredUrl) 
     H.h2 $ H.a H.! A.href urlLink $ H.toHtml (title post)
     H.h4 $ H.toHtml ("Created "<> (date post))   

--html to show entire post with href title
showBlogPost :: BlogPost -> Text
showBlogPost post = do
  R.renderHtml $ do
   let filteredUrl = Data.Text.Lazy.filter (not . (`elem` ['"','[','/',']','\\'])) $ Data.Text.Lazy.pack $ show (url post)
   let urlLink = H.toValue filteredUrl
   H.h2 $ H.a H.! A.href urlLink $ H.toHtml (title post)
   H.h4 $ H.toHtml ("Created "<> (date post)) 
   H.p $ H.toHtml(textBody post)

--gets the exact time, creates the post and returns URL      
func :: Text -> Text -> ActionM Text
func title body = do
  time <- liftIO $ getZonedTime
  let date = Data.Text.Lazy.pack (formatTime defaultTimeLocale "%c" time)
  let url = Data.Text.Lazy.pack $ show time
  let filteredUrl = Data.Text.Lazy.filter (\x -> x/=' ') url
  let n = BlogPost title body date filteredUrl 
  liftIO $ newFile n
  return filteredUrl
  

main :: IO()
main = do 
  scotty 3000 routes 

routes :: ScottyM()
routes = do
  S.get "/" $ listLatestFive 
  S.get "/posts" $ listAll
  S.get "/authen" $ do S.html $ Data.Text.Lazy.pack $ Prelude.unlines $
                        [ "<form method=\"POST\" action=\"/authen\">"
                        , "<input type=\"password\" name=\"password\">"
                        , "<input type=\"submit\" name=\"enter password\" value=\"enter password\">"
                        , "</form>" ]
  S.post "/authen" $ do
    pass <- param "password"
    if (authenticate pass == True)
      then do S.html $ Data.Text.Lazy.pack $ Prelude.unlines $
                        [ "<form method=\"POST\" action=\"/create\">"
                        , "<textarea rows=\"1\" cols=\"50\" name=\"title\">Enter title here</textarea>"
                        , "<textarea rows=\"4\" cols=\"50\" name=\"textbody\">Enter body of blog post here</textarea>"
                        , "<input type=\"submit\" name=\"create new post!\" value=\"create new post\">"
                        , "</form>" ]
      else do S.html "no blog post for you!"
 
  
  S.post "/create" $ do 
    heading <- param "title"
    body <- param "textbody"
    postUrl <- func heading body
    redirect ("/" <> postUrl)

  S.get "/:id" $ do
    url <- param "id"
    blog <- retrieveBlogPost url 
    let textView = read blog :: BlogPost
    html $ showBlogPost textView

