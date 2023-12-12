test

gitcreds_set()
PAT="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"
gitcreds_get()

credentials::set_github_pat("PAT")


usethis::use_git_config(user.name = "HeatherWelch", user.email = "heather.welch@noaa.gov")
usethis::create_github_token() 
credentials::set_github_pat("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")
