test

gitcreds_set()
PAT="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"
gitcreds_get()

credentials::set_github_pat("PAT")


usethis::use_git_config(user.name = "HeatherWelch", user.email = "heather.welch@noaa.gov")
usethis::create_github_token() 
credentials::set_github_pat("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")


#####
library(usethis)
library(gitcreds)
gitcreds::gitcreds_set("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")
gitcreds::gitcreds_get()
gh::gh_whoami()
usethis::git_sitrep()
usethis::gh_token_help()


####
gitcreds::gitcreds_set()
create_github_token()
usethis::gh_token_help() 
