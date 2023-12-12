test

gitcreds_set()

gitcreds_get()

credentials::set_github_pat("PAT")


usethis::use_git_config(user.name = "HeatherWelch", user.email = "heather.welch@noaa.gov")
usethis::create_github_token() 


#####
library(usethis)
library(gitcreds)

gitcreds::gitcreds_get()
gh::gh_whoami()
usethis::git_sitrep()
usethis::gh_token_help()


####
library(usethis)
library(gitcreds)
gitcreds::gitcreds_set()
create_github_token()
usethis::gh_token_help() 
gitcreds_get("https://github.com/HeatherWelch")
gitcreds::gitcreds_set("https://github.com/HeatherWelch")
