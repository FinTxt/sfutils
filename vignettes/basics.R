## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
# Text from: https://www.reuters.com/article/us-indonesia-economy-cenbank/indonesia-central-bank-schedules-extra-board-meeting-for-wednesday-idUSKCN1IQ1R6?il=0
txt <- "JAKARTA (Reuters) - Indonesia’s central bank will hold an additional meeting of its board of governors on Wednesday to discuss economic and monetary conditions, Bank Indonesia said in a statement on Friday. “The additional meeting will discuss recent economic and monetary conditions as well as future prospects,” the statement said. It did not elaborate. It said the meeting would not replace the regular monthly monetary policy meeting. The central bank on May 17 raised its benchmark interest rate for the first time since November 2014 in a bid to bolster the fragile rupiah and stem an outflow of capital. The currency has, however, remained weak and was trading near its lowest since October, 2015, this week."

## ------------------------------------------------------------------------
doc <- Document(txt)
doc

## ------------------------------------------------------------------------
doc <- do_fingerprint_document(txt)
doc

## ------------------------------------------------------------------------
doc_fp <- fingerprint(doc)
doc_uuid <- uuid(doc)

# Create a Document
doc <- Document(txt, fingerprint = doc_fp, uuid = doc_uuid)
doc

## ------------------------------------------------------------------------
trm <- do_fingerprint_term("jaguar")

## ------------------------------------------------------------------------
cntxt <- get_context(trm)
names(cntxt)

## ------------------------------------------------------------------------
trms <- get_similar_terms(trm)
names(trms)

## ------------------------------------------------------------------------
contextId <- cntxt$species$context_id
trms <- get_similar_terms(trm, contextId = contextId)
names(trms)

## ------------------------------------------------------------------------
# Sources
# https://www.reuters.com/article/us-oil-opec-exclusive/opec-russia-prepared-to-raise-oil-output-under-u-s-pressure-idUSKCN1IQ0Q6
# https://www.reuters.com/article/us-opec-meeting-cuts-duration/opec-non-opec-ministers-recommend-9-month-oil-cut-extension-idUSKBN1DT2HA
# https://www.reuters.com/article/us-opec-oil-barkindo/opecs-barkindo-says-opec-non-opec-agreement-rescued-oil-industry-idUSKBN1HR1VE
# https://www.reuters.com/article/us-oil-opec/more-countries-needed-in-opec-non-opec-pact-uaes-al-mazrouei-idUSKBN1HR0O6
filt <- do_create_filter(
  "crude",
  positive = c(
    "ST PETERSBURG/DUBAI (Reuters) - Saudi Arabia and Russia are discussing raising OPEC and non-OPEC oil production by some 1 million barrels a day, sources said, while OPEC’s chief said a complaint from U.S. President Donald Trump over high prices had triggered the idea of upping output. FILE PHOTO: A flag with the Organization of the Petroleum Exporting Countries (OPEC) logo is seen during a meeting of OPEC and non-OPEC producing countries in Vienna, Austria September 22, 2017. REUTERS/Leonhard Foeger/File Photo
Riyadh and Moscow are prepared to ease output cuts to calm consumer worries about supply adequacy, their energy ministers said on Friday, with Saudi Arabia’s Khalid al-Falih adding that any such move would be gradual so as not to shock the market. Raising production would ease 17 months of strict supply curbs amid concerns that a price rally has gone too far, with oil LCOc1 having hit its highest since late 2014 at $80.50 a barrel this month. OPEC began a discussion about easing production cuts following a critical tweet from Trump, OPEC’s Secretary-General Mohammad Barkindo said. Trump tweeted last month that OPEC had “artificially” boosted oil prices.",
  "VIENNA (Reuters) - A ministerial committee of OPEC and non-OPEC producers including Russia and Saudi Arabia recommended on Wednesday that OPEC and non-OPEC allies extend oil production cuts by nine months at a meeting the following day. Russian Energy Minister Alexander Novak arrives for a meeting with OPEC oil ministers at OPEC's headquarters in Vienna, Austria, November 29, 2017. REUTERS/Heinz-Peter Bader
“That’s one of the recommendations,” Kuwait’s Oil Minister Essam al-Marzouq told reporters when asked whether the committee had agreed on a nine-month extension. An OPEC source also confirmed that the ministerial meeting agreed with the earlier proposal to extend production cuts until the end of 2018.",
  "JEDDAH, Saudi Arabia (Reuters) - OPEC Secretary-General Mohammad Barkindo said on Friday members of the oil producers group were friends of the United States and have a vested interest in its growth and prosperity.Barkindo made his remarks after U.S. President Donald Trump earlier sent a tweet criticizing OPEC over high oil prices. “The Declaration of Cooperation entered into by 24 producing countries in Dec. 2016 and implemented faithfully since 2017 has not only arrested the decline but rescued the oil industry from imminent collapse,” Barkindo said.",
  "FRANKFURT (Reuters) - Further oil producers need to join Organization of the Petroleum Exporting Countries (OPEC) and non-OPEC producers in curbing supply, UAE oil minister Suhail Mohamed Al Mazrouei told a German newspaper. OPEC members, Russia and other non-OPEC producers have reduced output since January 2017 aiming to reduce inventories and support prices. The pact runs until the end of this year, and an OPEC meeting in June in Vienna will see participants decide their next course of action."
  )
)

## ---- fig.show="hold", fig.height=6, fig.width = 6-----------------------
crude_term <- do_fingerprint_term("petroleum")
plot(filt)
plot(filt, crude_term)

## ------------------------------------------------------------------------
data("fps_train")
fps <- as.collection(fps_train$fingerprints)
# To collection --> binary matrix
fps_col <- as.matrix(fps)

# Compare documents
cmp <- do_compare(fps_col, filt)
# To data.frame
cmp_df <- as.data.frame(cmp)
names(cmp_df) <- "filter"

# Order
ordrd <- order(cmp_df$filter, decreasing = TRUE)

# Get uuids of 10 most similar documents
uuids_sim <- row.names(cmp_df)[ordrd[1:10]]

# Retrieve texts for those documents
texts_sim <- as.list(do_search_collection(fps, uuids_sim))

# Get texts
cat(text(texts_sim[[1]]))
cat("\n")
cat(text(texts_sim[[2]]))

## ------------------------------------------------------------------------
data("fps_train")
tmp <- fps_train$fingerprints[1:10]
# Print a small subset
str(head(tmp))

## ------------------------------------------------------------------------
# Turn the list into a collection
col <- as.collection(tmp)
col

## ------------------------------------------------------------------------
col <- Collection(tmp)
col

## ------------------------------------------------------------------------
# Index
col[[1]]

## ------------------------------------------------------------------------
# Subset
col_ss <- col[1:5]
is(col_ss) # Not a Collection object

col_ss <- as.collection(col_ss)

## ------------------------------------------------------------------------
col_unlist <- as.list(col)
col_unlist <- entries(col) # We use this function to retrieve the list stored in the entries slot of the S4 class

## ------------------------------------------------------------------------
id <- uuid(entries(col)[[6]])
do_search_collection(col, id)

## ------------------------------------------------------------------------
ids <- c(uuid(entries(col)[[6]]), uuid(entries(col)[[7]]))
do_search_collection(col, ids)

## ------------------------------------------------------------------------
col_sbm <- as.matrix(col)
col_sbm[1:5, 1:5]

## ------------------------------------------------------------------------
ref <- do_fingerprint_term("petroleum")
do_compare(col_sbm, ref)

