library(usethis)

# I'm using a temp folder, but you should use something else like
# "C:/Users/vegayon/Documents/boringpkg"

# Creating the package
usethis::create_package(
  path = "D:/EIC-Code/01-R/doipkg",
  roxygen = TRUE,
  rstudio = TRUE,
  fields = list(
    Package = "doipkg",
    Version = "0.0.0.9000",
    Title = "A doi package",
    Description = "A doi package"
  )
)

# We can inspect what things it created:
list.files(tmp_folder, recursive = TRUE)