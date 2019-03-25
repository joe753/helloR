Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_191/")


install.packages(c("twitteR", "RCurl", "RJSONIO", "stringr", "streamR", "ROAuth"))


api_key = "fyrpnvjXBjpf1pPFEOL0m08ep"
api_secret = "sVPqcKBg6o9MBaE8pO3cDMIXZuR7ccDernAg08m81XWxovfLWu"
token = "1108923973427879939-119vJBmHIMaJb1jjVFKfUG4GPUZUSY"
token_secret = "JksRettfNZ7l0u7kfan50kuad1srDFIHQqfl1b4iPekrc"
setup_twitter_oauth(api_key, api_secret, token, token_secret)


install.packages(c("rJava", "memoise", "KoNLP"))

