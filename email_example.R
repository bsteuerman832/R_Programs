# Vector of a bunch of emails
emails <- c("john.doe@ivyleague.edu", "education@world.gov", "dalai.lama@peace.org",
            "invalid.edu", "quant@bigdatacollege.edu", "cookie.monster@sesame.tv")

# Use grepl() to match for .edu addresses
grepl(pattern = "@.*\\.edu$", emails)

# Use grep() to match for .edu addresses more robustly, save result to hits as numbers
hits <- grep(pattern = "@.*\\.edu$", emails)

# Subset emails using hits
emails[hits]

# Use sub() to convert the email domains to datacamp.edu
sub(pattern = "@.*\\.edu$", replacement = "@datacamp.edu", emails)