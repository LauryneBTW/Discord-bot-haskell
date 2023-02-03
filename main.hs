import Discord
import Discord.Types
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  (token, prefix) <- loadConfig
  withManager $ \m -> do
    void $ runDiscord m $ def
      { discordToken = token
      , discordOnMessageCreate = onMessageCreate prefix
      }

loadConfig :: IO (Token, Text)
loadConfig = do
  token <- T.strip <$> readFile "config/token.txt"
  prefix <- T.strip <$> readFile "config/prefix.txt"

  return (token, prefix)

onMessageCreate :: Text -> DiscordHandle -> Message -> IO ()
onMessageCreate prefix dh message = do
  if not (messageAuthorIsWebhook message) && messageAuthorIsUser message
    then do
      let content = messageContent message
      if T.isPrefixOf prefix content
        then do
          let command = T.stripStart (T.drop (T.length prefix) content)
          if command == "ping"
            then do
              void $ discordCreateMessage dh (messageChannel message) (T.pack (show (discordShardLatency dh)))
            else return ()
        else return ()
    else return ()
