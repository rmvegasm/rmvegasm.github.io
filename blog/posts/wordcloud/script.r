## ----r------------------------------------------------------------------------
library('rtweet')
# Authenticate the currently logged-in user and store credentials, this needs to
# be done only once per machine:
# rtweet::auth_setup_default()

# Do not perform query if results are already stored:
tbl_file = 'tweets_tbl.rds'
tweets_file = 'tweets.csv'

if (file.exists(tweets_file)) {
  tweets = read.csv(tweets_file)
} else {
  tweets = rtweet::search_tweets(
    'chile -filter:quote -filter:media lang:es',
    n = 2000,
    include_rts = FALSE,
    retryonratelimit = TRUE
  )
  # let's save the tibble just in case and write a csv ommiting list cols
  saveRDS(tweets, tbl_file)
  tweets = tweets[, sapply(tweets, class) != 'list'] |> as.data.frame()
  write.csv(tweets, tweets_file, row.names = FALSE)
}












## ----r------------------------------------------------------------------------
is_link = function (x) grepl('^[[:lower:]]+://.+', x)
is_hashtag = function (x) grepl('^#.+', x)
is_mention = function (x) grepl('^@.+', x)
is_number = function (x) grepl('^[[:digit:]]+[[:punct:]]*$', x)










## ----r------------------------------------------------------------------------
extract_words = function (x) {
  x = x[!is_link(x)]
  x = x[!is_hashtag(x)]
  x = x[!is_mention(x)]
  x = x[!is_number(x)]
  x = lapply(x, gsub, pattern = '[^[:alnum:]]', replacement = '') |> unlist()
  x = x[x != '']
  tolower(x)
}




## ----r------------------------------------------------------------------------
clean_tweets = tweets[['full_text']] |>
  strsplit(split = '[[:space:]+]') |>
  lapply(extract_words)

words = lapply(clean_tweets, function (x) {
  n = nchar(x)
  x[n >= 5 & n <= 10]
})

word_bag = unlist(words)




## ----r------------------------------------------------------------------------
word_bag = local({
  query = 'chile'
  word_bag[!(word_bag %in% query)]
})


## ----r------------------------------------------------------------------------
library('wordcloud2')
library('wesanderson')

wf = local({
  wb = table(word_bag)
  word = names(wb)
  freq = as.numeric(wb)
  data.frame(word, freq)[freq >= 10, ]
})

# Nice colors from 'The Darjeeling Express'
clrs = rep(wesanderson::wes_palette('Darjeeling1', 5), length.out = nrow(wf))

wordcloud2::wordcloud2(wf,
  background = 'transparent',
  color = clrs,
  size = .3)

